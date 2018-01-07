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
  , IndexedEvent(..)
  , Tagged
  , module Generics.SOP
  ) where

import           Control.Monad                          ((<=<))
import           Data.List                              (length, uncons)
import           Data.Tagged                            (Tagged)
import qualified Data.Text                              as T
import qualified Data.Text.Lazy                         as LT
import qualified Data.Text.Lazy.Builder                 as B
import qualified Data.Text.Lazy.Encoding                as LT
import           Text.Parsec.Text                       as P

import           Network.Ethereum.Unit
import           Network.Ethereum.Web3.Address          (Address)
import           Network.Ethereum.Web3.Contract
import           Network.Ethereum.Web3.Encoding
import           Network.Ethereum.Web3.Encoding.Event
import           Network.Ethereum.Web3.Encoding.Int
import           Network.Ethereum.Web3.Encoding.Vector
import           Network.Ethereum.Web3.Encoding.Generic
import           Network.Ethereum.Web3.Internal
import           Network.Ethereum.Web3.JsonAbi
import           Network.Ethereum.Web3.Provider
import           Network.Ethereum.Web3.Types

import           Control.Monad                          (replicateM)

import           Data.Aeson
import           Data.ByteArray                         (Bytes)
import           Data.List                              (groupBy, sortBy)
import           Data.Monoid                            (mconcat, (<>))
import           Data.Text                              (Text, isPrefixOf)

import           Generics.SOP
import qualified GHC.Generics                           as GHC

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

toHSType :: SolidityType -> TypeQ
toHSType s = case s of
    SolidityBool -> conT (mkName "Bool")
    SolidityAddress -> conT (mkName "Address")
    SolidityUint n -> appT (conT (mkName "UIntN")) (numLit n)
    SolidityInt n -> appT (conT (mkName "IntN")) (numLit n)
    SolidityString ->  conT (mkName "Text")
    SolidityBytesN n -> appT (conT (mkName "BytesN")) (numLit n)
    SolidityBytesD ->  conT (mkName "BytesD")
    SolidityVector ns a -> expandVector ns a
    SolidityArray a -> appT listT $ toHSType a
  where
    numLit n = litT (numTyLit $ toInteger n)
    expandVector :: [Int] -> SolidityType -> TypeQ
    expandVector ns a = case uncons ns of
      Just (n, rest) ->
        if length rest == 0
          then (conT $ mkName "Vector") `appT` numLit n `appT` toHSType a
          else (conT $ mkName "Vector") `appT` numLit n `appT` expandVector rest a
      _ -> error $ "impossible Nothing branch in `expandVector`: " ++ show ns ++ " " ++ show a

typeQ :: Text -> TypeQ
typeQ t = case parseSolidityType t of
  Left e -> error $ "Unable to parse solidity type: " ++ show e
  Right ty -> toHSType ty

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
    [ funD' (mkName "fromDataParser") [] [|decodeEvent|] ]

eventFilterD :: String -> Int -> [DecQ]
eventFilterD topic0 n =
  let addr = mkName "a"
      indexedArgs = replicate n Nothing :: [Maybe String]
  in [ funD' (mkName "eventFilter") [wildP, varP addr]
       [|Filter (Just $(varE addr))
                (Just $ [Just topic0] <> indexedArgs)
                Latest
                Latest
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

mkEvent :: Declaration -> Q [Dec]
mkEvent ev@(DEvent name inputs anonymous) = sequence
    [ dataD' indexedName (normalC indexedName (map (toBang <=< tag) indexedArgs)) derivingD
    , instanceD' indexedName (conT (mkName "Generic")) []
    , dataD' nonIndexedName (normalC nonIndexedName (map (toBang <=< tag) nonIndexedArgs)) derivingD
    , instanceD' nonIndexedName (conT (mkName "Generic")) []
    , dataD' allName (recC allName (map (\(n, a) -> ((\(b,t) -> return (n,b,t)) <=< toBang <=< typeQ $ a)) allArgs)) derivingD
    , instanceD' allName (conT (mkName "Generic")) []
    , instanceD (cxt []) (return $ (ConT $ mkName "IndexedEvent") `AppT` ConT indexedName `AppT` ConT nonIndexedName `AppT` ConT allName)
        [funD' (mkName "isAnonymous") [] [|const anonymous|]]
    , instanceD' allName eventT (eventFilterD (T.unpack $ eventId ev) (length indexedArgs))

    ]
  where
    toBang ty = bangType (bang sourceNoUnpack sourceStrict) (return ty)
    tag (n, ty) = AppT (AppT (ConT $ mkName "Tagged") (LitT $ NumTyLit n)) <$> typeQ ty
    labeledArgs = zip [1..] inputs
    indexedArgs = map (\(n, ea) -> (n, eveArgType ea)) . filter (eveArgIndexed . snd) $ labeledArgs
    indexedName = mkName $ toUpperFirst (T.unpack name) <> "Indexed"
    nonIndexedArgs = map (\(n, ea) -> (n, eveArgType ea)) . filter (not . eveArgIndexed . snd) $ labeledArgs
    nonIndexedName = mkName $ toUpperFirst (T.unpack name) <> "NonIndexed"
    allArgs = makeArgs name $ map (\i -> (eveArgName i, eveArgType i)) inputs
    allName = mkName $ toUpperFirst (T.unpack name)
    derivingD = [mkName "Show", mkName "Eq", mkName "Ord", ''GHC.Generic]
    eventT = conT (mkName "Event")

-- | this function gives appropriate names for the accessors in the following way
-- | argName -> evArgName
-- | arg_name -> evArg_name
-- | _argName -> evArgName
-- | "" -> evi , for example Transfer(address, address uint256) ~> Transfer {transfer1 :: address, transfer2 :: address, transfer3 :: Integer}
makeArgs :: T.Text -> [(T.Text, T.Text)] -> [(Name, T.Text)]
makeArgs prefix ns = go 1 ns
  where
    prefixStr = toLowerFirst . T.unpack $ prefix
    go :: Int -> [(T.Text, T.Text)] -> [(Name, T.Text)]
    go i [] = []
    go i ((h, ty) : tail) = if T.null h
                        then (mkName $  prefixStr ++ show i, ty) : go (i + 1) tail
                        else (mkName . (++) prefixStr . toUpperFirst . (\t -> if head t == '_' then drop 1 t else t) . T.unpack $ h, ty) : go (i + 1) tail

-- | Method delcarations maker
mkFun :: Declaration -> Q [Dec]
mkFun fun@(DFunction name constant inputs outputs) = (++)
  <$> funWrapper constant funName dataName inputs outputs
  <*> sequence
        [ dataD' dataName (normalC dataName bangInput) derivingD
        , instanceD' dataName (conT (mkName "Generic")) []
        , instanceD' dataName  (conT (mkName "Method"))
          [funD' (mkName "selector") [] [|const mIdent|]]
        ]
  where mIdent    = T.unpack (methodId $ fun{funName = T.replace "'" "" name})
        dataName  = mkName (toUpperFirst (T.unpack $ name <> "Data"))
        funName   = mkName (toLowerFirst (T.unpack name))
        bangInput = fmap funBangType inputs
        derivingD = [mkName "Show", mkName "Eq", mkName "Ord", ''GHC.Generic]

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
