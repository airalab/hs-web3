{-# LANGUAGE CPP              #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE TemplateHaskell  #-}

-- |
-- Module      :  Network.Ethereum.Web3.TH
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- TemplateHaskell based Ethereum contract ABI
-- methods & event generator for Haskell native API.
--
-- @
-- [abiFrom|data/sample.json|]
--
-- main = do
--     runWeb3 $ event "0x..." $
--        \(Action2 n x) -> liftIO $ do print n
--                                      print x
--     wait
--   where wait = threadDelay 1000000 >> wait
-- @
--
module Network.Ethereum.Web3.TH (
  -- ** Quasiquoter's
    abi
  , abiFrom
  -- ** Used by TH data types
  , Bytes
  , Text
  , Singleton(..)
  , ABIEncoding(..)
  ) where

import qualified Data.Attoparsec.Text                 as P
import qualified Data.Text                            as T
import qualified Data.Text.Lazy                       as LT
import qualified Data.Text.Lazy.Builder               as B
import qualified Data.Text.Lazy.Encoding              as LT

import           Network.Ethereum.Unit
import           Network.Ethereum.Web3.Address        (Address)
import           Network.Ethereum.Web3.Contract
import           Network.Ethereum.Web3.Encoding
import           Network.Ethereum.Web3.Encoding.Tuple
import           Network.Ethereum.Web3.Internal
import           Network.Ethereum.Web3.JsonAbi
import           Network.Ethereum.Web3.Provider
import           Network.Ethereum.Web3.Types

import           Control.Monad                        (replicateM)

import           Data.Aeson
import           Data.ByteArray                       (Bytes)
import           Data.List                            (groupBy, sortBy)
import           Data.Monoid                          (mconcat, (<>))
import           Data.Text                            (Text, isPrefixOf)

import           GHC.Generics

import           Language.Haskell.TH
import           Language.Haskell.TH.Lib
import           Language.Haskell.TH.Quote

-- | Read contract ABI from file
abiFrom :: QuasiQuoter
abiFrom = quoteFile abi

-- | QQ reader for contract ABI
abi :: QuasiQuoter
abi = QuasiQuoter
  { quoteDec  = quoteAbiDec
  , quoteExp  = quoteAbiExp
  , quotePat  = undefined
  , quoteType = undefined
  }

-- | Instance declaration with empty context
instanceD' :: Name -> TypeQ -> [DecQ] -> DecQ
instanceD' name insType =
    instanceD (cxt []) (appT insType (conT name))

-- | Simple data type declaration with one constructor
dataD' :: Name -> ConQ -> [Name] -> DecQ
dataD' name rec derive =
#if MIN_VERSION_template_haskell(2,12,0)
    dataD (cxt []) name [] Nothing [rec] [derivClause Nothing (conT <$> derive)]
#else
    dataD (cxt []) name [] Nothing [rec] $ cxt (conT <$> derive)
#endif

-- | Simple function declaration
funD' :: Name -> [PatQ] -> ExpQ -> DecQ
funD' name p f = funD name [clause p (normalB f) []]

-- | ABI and Haskell types association
typeQ :: Text -> TypeQ
typeQ typ | T.any (== '[') typ = appT listT (go (T.takeWhile (/= '[') typ))
          | otherwise          = go typ
  where go x | "string"  == x         = conT (mkName "Text")
             | "address" == x         = conT (mkName "Address")
             | "bytes"   == x         = conT (mkName "BytesD")
             | "bool"    == x         = conT (mkName "Bool")
             | "bytes" `isPrefixOf` x = appT (conT (mkName "BytesN"))
                                             (numLit (T.drop 5 x))
             | "int"   `isPrefixOf` x = conT (mkName "Integer")
             | "uint"  `isPrefixOf` x = conT (mkName "Integer")
             | otherwise = fail ("Unknown type: " ++ T.unpack x)
        numLit n = litT (numTyLit (read (T.unpack n)))

-- | Event argument to TH type
eventBangType :: EventArg -> BangTypeQ
eventBangType (EventArg _ typ _) =
    bangType (bang sourceNoUnpack sourceStrict) (typeQ typ)

-- | Function argument to TH type
funBangType :: FunctionArg -> BangTypeQ
funBangType (FunctionArg _ typ) =
    bangType (bang sourceNoUnpack sourceStrict) (typeQ typ)

-- | Solidity dynamic type predicate
isDynType :: Text -> Bool
isDynType "bytes"  = True
isDynType "string" = True
isDynType x | T.any (== '[') x = True
            | otherwise        = False

eventEncodigD :: Name -> [EventArg] -> [DecQ]
eventEncodigD eventName args =
    [ funD' (mkName "toDataBuilder")  []
        [|error "Event to data conversion isn't available!"|]
    , funD' (mkName "fromDataParser") [] fromDataP ]
  where
    indexed = map eveArgIndexed args
    newVars = replicateM (length args) (newName "t")

    parseArg v = bindS (varP v) [|fromDataParser|]

    parseData []   = []
    parseData [v]  = pure $ bindS (varP v) [|unSingleton <$> fromDataParser|]
    parseData vars = pure $ bindS (tupP (varP <$> vars)) [|fromDataParser|]

    fromDataP = do
        vars <- zip indexed <$> newVars
        let ixVars   = [v | (isIndexed, v) <- vars, isIndexed]
            noIxVars = [v | (isIndexed, v) <- vars, not isIndexed]
            expVars  = [varE v | (_, v) <- vars]
        doE $ fmap parseArg ixVars
           ++ parseData noIxVars
           ++ [noBindS [|return $(appsE (conE eventName : expVars))|]]

funEncodigD :: Name -> Int -> String -> [DecQ]
funEncodigD funName paramLen ident =
    [ funDtoDataB
    , funD' (mkName "fromDataParser") []
        [|error "Function from data conversion isn't available!"|] ]
  where
    newVars = replicateM paramLen (newName "t")
    sVar    = mkName "a"
    funDtoDataB
        | paramLen == 0 = funD' (mkName "toDataBuilder") [conP funName []] [|ident|]
        | paramLen == 1 = funD' (mkName "toDataBuilder")
                            [conP funName [varP sVar]]
                                [|ident <> toDataBuilder (Singleton $(varE sVar))|]
        | otherwise = do
            vars <- newVars
            funD' (mkName "toDataBuilder")
              [conP funName $ fmap varP vars]
              [|ident <> toDataBuilder $(tupE $ fmap varE vars)|]

eventFilterD :: String -> Int -> [DecQ]
eventFilterD topic0 n =
  let addr = mkName "a"
      indexedArgs = replicate n Nothing :: [Maybe String]
  in [ funD' (mkName "eventFilter") [wildP, varP addr]
       [|Filter (Just $(varE addr))
                (Just $ [Just topic0] <> indexedArgs)
                Nothing
                Nothing
       |]
     ]

funWrapper :: Bool
           -- ^ Is constant?
           -> Name
           -- ^ Function name
           -> Name
           -- ^ Function data name
           -> [FunctionArg]
           -- ^ Parameters
           -> Maybe [FunctionArg]
           -- ^ Results
           -> Q [Dec]
funWrapper c name dname args result = do
    a : b : vars <- replicateM (length args + 2) (newName "t")
    let params = appsE $ conE dname : fmap varE vars

    sequence $ if c
        then
          [ sigD name $ [t|Provider $p =>
                            $(arrowing $ [t|Call|] : inputT ++ [outputT])
                          |]
          , funD' name (varP <$> a : vars) $
              case result of
                Just [_] -> [|unSingleton <$> call $(varE a) Latest $(params)|]
                _        -> [|call $(varE a) Latest $(params)|]
          ]

        else
          [ sigD name $ [t|(Provider $p) =>
                            $(arrowing $ [t|Call|] : inputT ++ [[t|Web3 $p TxHash|]])
                          |]
          , funD' name (varP <$> a : vars) $
                [|sendTx $(varE a) $(params)|] ]
  where
    p = varT (mkName "p")
    arrowing [x]      = x
    arrowing (x : xs) = [t|$x -> $(arrowing xs)|]
    inputT  = fmap (typeQ . funArgType) args
    outputT = case result of
        Nothing  -> [t|Web3 $p ()|]
        Just [x] -> [t|Web3 $p $(typeQ $ funArgType x)|]
        Just xs  -> let outs = fmap (typeQ . funArgType) xs
                    in  [t|Web3 $p $(foldl appT (tupleT (length xs)) outs)|]

-- | Event declarations maker
mkEvent :: Declaration -> Q [Dec]
mkEvent eve@(DEvent name inputs _) = sequence
    [ dataD' eventName eventFields derivingD
    , instanceD' eventName encodingT (eventEncodigD eventName inputs)
    , instanceD' eventName eventT    (eventFilterD (T.unpack $ eventId eve) indexedFieldsCount)
    ]
  where eventName   = mkName (toUpperFirst (T.unpack name))
        derivingD   = [mkName "Show", mkName "Eq", mkName "Ord", ''Generic]
        eventFields = normalC eventName (eventBangType <$> inputs)
        encodingT   = conT (mkName "ABIEncoding")
        eventT      = conT (mkName "Event")
        indexedFieldsCount = length . filter eveArgIndexed $ inputs

-- | Method delcarations maker
mkFun :: Declaration -> Q [Dec]
mkFun fun@(DFunction name constant inputs outputs) = (++)
  <$> funWrapper constant funName dataName inputs outputs
  <*> sequence
        [ dataD' dataName (normalC dataName bangInput) derivingD
        , instanceD' dataName encodingT
            (funEncodigD dataName (length inputs) mIdent)
        , instanceD' dataName methodT [] ]
  where mIdent    = T.unpack (methodId $ fun{funName = T.replace "'" "" name})
        dataName  = mkName (toUpperFirst (T.unpack $ name <> "Data"))
        funName   = mkName (toLowerFirst (T.unpack name))
        bangInput = fmap funBangType inputs
        derivingD = [mkName "Show", mkName "Eq", mkName "Ord", ''Generic]
        encodingT = conT (mkName "ABIEncoding")
        methodT   = conT (mkName "Method")

escape :: [Declaration] -> [Declaration]
escape = concat . escapeNames . groupBy fnEq . sortBy fnCompare
  where fnEq (DFunction n1 _ _ _) (DFunction n2 _ _ _) = n1 == n2
        fnEq _ _                                       = False
        fnCompare (DFunction n1 _ _ _) (DFunction n2 _ _ _) = compare n1 n2
        fnCompare _ _                                       = GT

escapeNames :: [[Declaration]] -> [[Declaration]]
escapeNames = fmap go
  where go (x : xs) = x : zipWith appendToName xs hats
        hats = [T.replicate n "'" | n <- [1..]]
        appendToName dfn addition = dfn { funName = funName dfn <> addition }

-- | Declaration parser
mkDecl :: Declaration -> Q [Dec]
mkDecl x@DFunction{} = mkFun x
mkDecl x@DEvent{}    = mkEvent x
mkDecl _             = return []

-- | ABI to declarations converter
quoteAbiDec :: String -> Q [Dec]
quoteAbiDec abi_string =
    case decode abi_lbs of
        Just (ContractABI abi) -> concat <$> mapM mkDecl (escape abi)
        _                      -> fail "Unable to parse ABI!"
  where abi_lbs = LT.encodeUtf8 (LT.pack abi_string)

-- | ABI information string
quoteAbiExp :: String -> ExpQ
quoteAbiExp abi_string = stringE $
    case eitherDecode abi_lbs of
        Left e    -> "Error: " ++ show e
        Right abi -> show (abi :: ContractABI)
  where abi_lbs = LT.encodeUtf8 (LT.pack abi_string)
