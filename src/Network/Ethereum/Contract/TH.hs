{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

-- |
-- Module      :  Network.Ethereum.Contract.TH
-- Copyright   :  Alexander Krupenkin 2016-2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- TemplateHaskell based generator for Ethereum contract ABI.
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

module Network.Ethereum.Contract.TH (abi, abiFrom) where

import           Control.Monad                     (replicateM, (<=<))
import           Data.Aeson                        (eitherDecode)
import           Data.Default                      (Default (..))
import           Data.List                         (groupBy, sortBy, uncons)
import           Data.Monoid                       ((<>))
import           Data.Tagged                       (Tagged)
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import qualified Data.Text.Lazy                    as LT
import qualified Data.Text.Lazy.Encoding           as LT
import           Generics.SOP                      (Generic)
import qualified GHC.Generics                      as GHC (Generic)
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote

import           Data.String.Extra                 (toLowerFirst, toUpperFirst)
import           Network.Ethereum.ABI.Class        (ABIGet, ABIPut,
                                                    ABIType (..))
import           Network.Ethereum.ABI.Event        (IndexedEvent (..))
import           Network.Ethereum.ABI.Json         (ContractABI (..),
                                                    Declaration (..),
                                                    EventArg (..),
                                                    FunctionArg (..),
                                                    SolidityType (..), eventId,
                                                    methodId, parseSolidityType)
import           Network.Ethereum.ABI.Prim.Address (Address)
import           Network.Ethereum.ABI.Prim.Bytes   (Bytes, BytesN)
import           Network.Ethereum.ABI.Prim.Int     (IntN, UIntN)
import           Network.Ethereum.ABI.Prim.List    (ListN)
import           Network.Ethereum.ABI.Prim.Tagged  ()
import           Network.Ethereum.ABI.Prim.Tuple   (Singleton (..))
import           Network.Ethereum.Contract.Method  (Method (..), call)
import           Network.Ethereum.Web3.Provider    (Web3)
import           Network.Ethereum.Web3.Types       (Call, DefaultBlock (..),
                                                    Filter (..), TxHash)

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
    SolidityBool        -> conT ''Bool
    SolidityAddress     -> conT ''Address
    SolidityUint n      -> appT (conT ''UIntN) (numLit n)
    SolidityInt n       -> appT (conT ''IntN) (numLit n)
    SolidityString      -> conT ''Text
    SolidityBytesN n    -> appT (conT ''BytesN) (numLit n)
    SolidityBytes       -> conT ''Bytes
    SolidityVector ns a -> expandVector ns a
    SolidityArray a     -> appT listT $ toHSType a
  where
    numLit n = litT (numTyLit $ toInteger n)
    expandVector :: [Int] -> SolidityType -> TypeQ
    expandVector ns a = case uncons ns of
      Just (n, rest) ->
        if length rest == 0
          then (conT ''ListN) `appT` numLit n `appT` toHSType a
          else (conT ''ListN) `appT` numLit n `appT` expandVector rest a
      _ -> error $ "Impossible Nothing branch in `expandVector`: " ++ show ns ++ " " ++ show a

typeQ :: Text -> TypeQ
typeQ t = case parseSolidityType t of
  Left e   -> error $ "Unable to parse solidity type: " ++ show e
  Right ty -> toHSType ty

-- | Function argument to TH type
funBangType :: FunctionArg -> BangTypeQ
funBangType (FunctionArg _ typ) =
    bangType (bang sourceNoUnpack sourceStrict) (typeQ typ)

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
    a : _ : vars <- replicateM (length args + 2) (newName "t")
    let params = appsE $ conE dname : fmap varE vars

    sequence $ if c
        then
          [ sigD name $ [t|$(arrowing $ [t|Call|] : inputT ++ [outputT])|]
          , funD' name (varP <$> a : vars) $
              case result of
                Just [_] -> [|unSingleton <$> call $(varE a) Latest $(params)|]
                _        -> [|call $(varE a) Latest $(params)|]
          ]

        else
          [ sigD name $ [t|$(arrowing $ [t|Call|] : inputT ++ [[t|Web3 TxHash|]])|]
          , funD' name (varP <$> a : vars) $
                [|sendTx $(varE a) $(params)|] ]
  where
    arrowing []       = error "Impossible branch call"
    arrowing [x]      = x
    arrowing (x : xs) = [t|$x -> $(arrowing xs)|]
    inputT  = fmap (typeQ . funArgType) args
    outputT = case result of
        Nothing  -> [t|Web3 ()|]
        Just [x] -> [t|Web3 $(typeQ $ funArgType x)|]
        Just xs  -> let outs = fmap (typeQ . funArgType) xs
                    in  [t|Web3 $(foldl appT (tupleT (length xs)) outs)|]

mkDecl :: Declaration -> Q [Dec]

mkDecl ev@(DEvent name inputs anonymous) = sequence
    [ dataD' indexedName (normalC indexedName (map (toBang <=< tag) indexedArgs)) derivingD
    , instanceD' indexedName (conT ''Generic) []
    , instanceD' indexedName (conT ''ABIType) [funD' 'isDynamic [] [|const False|]]
    , instanceD' indexedName (conT ''ABIGet) []
    , dataD' nonIndexedName (normalC nonIndexedName (map (toBang <=< tag) nonIndexedArgs)) derivingD
    , instanceD' nonIndexedName (conT ''Generic) []
    , instanceD' nonIndexedName (conT ''ABIType) [funD' 'isDynamic [] [|const False|]]
    , instanceD' nonIndexedName (conT ''ABIGet) []
    , dataD' allName (recC allName (map (\(n, a) -> ((\(b,t) -> return (n,b,t)) <=< toBang <=< typeQ $ a)) allArgs)) derivingD
    , instanceD' allName (conT ''Generic) []
    , instanceD (cxt [])
        (pure $ ConT ''IndexedEvent `AppT` ConT indexedName `AppT` ConT nonIndexedName `AppT` ConT allName)
        [funD' 'isAnonymous [] [|const anonymous|]]
    , instanceD (cxt [])
        (pure $ ConT ''Default `AppT` (ConT ''Filter `AppT` ConT allName))
        [funD' 'def [] [|Filter Nothing (Just topics) Latest Latest|] ]
    ]
  where
    topics    = [Just (T.unpack $ eventId ev)] <> replicate (length indexedArgs) Nothing
    toBang ty = bangType (bang sourceNoUnpack sourceStrict) (return ty)
    tag (n, ty) = AppT (AppT (ConT ''Tagged) (LitT $ NumTyLit n)) <$> typeQ ty
    labeledArgs = zip [1..] inputs
    indexedArgs = map (\(n, ea) -> (n, eveArgType ea)) . filter (eveArgIndexed . snd) $ labeledArgs
    indexedName = mkName $ toUpperFirst (T.unpack name) <> "Indexed"
    nonIndexedArgs = map (\(n, ea) -> (n, eveArgType ea)) . filter (not . eveArgIndexed . snd) $ labeledArgs
    nonIndexedName = mkName $ toUpperFirst (T.unpack name) <> "NonIndexed"
    allArgs = makeArgs name $ map (\i -> (eveArgName i, eveArgType i)) inputs
    allName = mkName $ toUpperFirst (T.unpack name)
    derivingD = [''Show, ''Eq, ''Ord, ''GHC.Generic]

-- | Method delcarations maker
mkDecl fun@(DFunction name constant inputs outputs) = (++)
  <$> funWrapper constant fnName dataName inputs outputs
  <*> sequence
        [ dataD' dataName (normalC dataName bangInput) derivingD
        , instanceD' dataName (conT ''Generic) []
        , instanceD' dataName (conT ''ABIType)
          [funD' 'isDynamic [] [|const False|]]
        , instanceD' dataName (conT ''ABIPut) []
        , instanceD' dataName (conT ''ABIGet) []
        , instanceD' dataName (conT ''Method)
          [funD' 'selector [] [|const mIdent|]]
        ]
  where mIdent    = T.unpack (methodId $ fun {funName = T.replace "'" "" name})
        dataName  = mkName (toUpperFirst (T.unpack $ name <> "Data"))
        fnName    = mkName (toLowerFirst (T.unpack name))
        bangInput = fmap funBangType inputs
        derivingD = [''Show, ''Eq, ''Ord, ''GHC.Generic]

mkDecl _ = return []

-- | this function gives appropriate names for the accessors in the following way
-- | argName -> evArgName
-- | arg_name -> evArg_name
-- | _argName -> evArgName
-- | "" -> evi , for example Transfer(address, address uint256) ~> Transfer {transfer1 :: address, transfer2 :: address, transfer3 :: Integer}
makeArgs :: Text -> [(Text, Text)] -> [(Name, Text)]
makeArgs prefix ns = go 1 ns
  where
    prefixStr = toLowerFirst . T.unpack $ prefix
    go :: Int -> [(Text, Text)] -> [(Name, Text)]
    go _ [] = []
    go i ((h, ty) : tail') = if T.null h
                        then (mkName $ prefixStr ++ show i, ty) : go (i + 1) tail'
                        else (mkName . (++ "_") . (++) prefixStr . toUpperFirst . T.unpack $ h, ty) : go (i + 1) tail'

escape :: [Declaration] -> [Declaration]
escape = concat . escapeNames . groupBy fnEq . sortBy fnCompare
  where fnEq (DFunction n1 _ _ _) (DFunction n2 _ _ _) = n1 == n2
        fnEq _ _                                       = False
        fnCompare (DFunction n1 _ _ _) (DFunction n2 _ _ _) = compare n1 n2
        fnCompare _ _                                       = GT

escapeNames :: [[Declaration]] -> [[Declaration]]
escapeNames = fmap go
  where go []       = []
        go (x : xs) = x : zipWith appendToName xs hats
        hats = [T.replicate n "'" | n <- [1..]]
        appendToName dfn addition = dfn { funName = funName dfn <> addition }

-- | ABI to declarations converter
quoteAbiDec :: String -> Q [Dec]
quoteAbiDec abi_string =
    case eitherDecode abi_lbs of
        Left e                -> fail $ "Error: " ++ show e
        Right (ContractABI a) -> concat <$> mapM mkDecl (escape a)
  where abi_lbs = LT.encodeUtf8 (LT.pack abi_string)

-- | ABI information string
quoteAbiExp :: String -> ExpQ
quoteAbiExp abi_string = stringE $
    case eitherDecode abi_lbs of
        Left e  -> "Error: " ++ show e
        Right a -> show (a :: ContractABI)
  where abi_lbs = LT.encodeUtf8 (LT.pack abi_string)
