{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

-- |
-- Module      :  Network.Ethereum.Contract.TH
-- Copyright   :  Aleksandr Krupenkin 2016-2020
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- Contract abstraction is a high level interface of web3 library.
--
-- The Application Binary Interface is the standard way to interact
-- with contracts in the Ethereum ecosystem. It can be described by
-- specially JSON file, like @ERC20.json@. This module use TemplateHaskell
-- for generation described in Abi contract methods and events. Helper
-- functions and instances inserted in haskell module and can be used in
-- another modules or in place.
--
-- @
-- import Network.Ethereum.Contract.TH
--
-- [abiFrom|examples/ERC20.json|]
--
-- main = do
--     runWeb3 $ event' def $
--        \(Transfer _ to val) -> liftIO $ do print to
--                                            print val
-- @
--
-- Full code example available in examples folder.
--

module Network.Ethereum.Contract.TH
    (
    -- * The contract quasiquoters
      abi
    , abiFrom
    ) where

import           Control.Applicative              ((<|>))
import           Control.Monad                    (replicateM, (<=<))
import qualified Data.Aeson                       as Aeson (encode)
import           Data.ByteArray                   (convert)
import           Data.Char                        (toLower, toUpper)
import qualified Data.Char                        as Char
import           Data.Default                     (Default (..))
import           Data.List                        (group, sort, uncons)
import           Data.Tagged                      (Tagged)
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import qualified Data.Text.Lazy                   as LT
import qualified Data.Text.Lazy.Encoding          as LT
import           Data.Tuple.OneTuple              (only)
import           Generics.SOP                     (Generic)
import qualified GHC.Generics                     as GHC (Generic)
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Lens.Micro                       (over, (^?), _head)
import           Lens.Micro.Aeson                 (key, _JSON, _String)

import           Data.Solidity.Abi                (AbiGet, AbiPut, AbiType (..))
import           Data.Solidity.Event              (IndexedEvent (..))
import           Data.Solidity.Prim               (Address, Bytes, BytesN, IntN,
                                                   ListN, UIntN)
import           Language.Solidity.Abi            (ContractAbi (..),
                                                   Declaration (..),
                                                   EventArg (..),
                                                   FunctionArg (..),
                                                   SolidityType (..),
                                                   eventId,
                                                   methodId,
                                                   parseSolidityEventArgType,
                                                   parseSolidityFunctionArgType)
import           Network.Ethereum.Account.Class   (Account (..))
import           Network.Ethereum.Api.Types       (DefaultBlock (..),
                                                   Filter (..), TxReceipt)
import qualified Network.Ethereum.Contract        as Contract (Contract (..))
import           Network.Ethereum.Contract.Method (Method (..))
import           Network.JsonRpc.TinyClient       (JsonRpc)

-- | Read contract Abi from file
abiFrom :: QuasiQuoter
abiFrom = quoteFile abi

-- | QQ reader for contract Abi
abi :: QuasiQuoter
abi = QuasiQuoter
    { quoteDec  = quoteAbiDec
    , quoteExp  = quoteAbiExp
    , quotePat  = undefined
    , quoteType = undefined }

-- | Instance declaration with empty context
instanceD' :: Name -> TypeQ -> [DecQ] -> DecQ
instanceD' name insType =
    instanceD (cxt []) (appT insType (conT name))

-- | Simple data type declaration with one constructor
dataD' :: Name -> ConQ -> [Name] -> DecQ
dataD' name rec' derive =
    dataD (cxt []) name [] Nothing [rec'] [derivClause Nothing (conT <$> derive)]

-- | Simple function declaration
funD' :: Name -> [PatQ] -> ExpQ -> DecQ
funD' name p f = funD name [clause p (normalB f) []]

-- | Abi and Haskell types association
toHSType :: SolidityType -> TypeQ
toHSType s = case s of
    SolidityBool        -> conT ''Bool
    SolidityAddress     -> conT ''Address
    SolidityUint n      -> appT (conT ''UIntN) (numLit n)
    SolidityInt n       -> appT (conT ''IntN) (numLit n)
    SolidityString      -> conT ''Text
    SolidityBytesN n    -> appT (conT ''BytesN) (numLit n)
    SolidityBytes       -> conT ''Bytes
    SolidityTuple as    -> foldl ( \b a -> appT b $ toHSType a ) ( tupleT (length as) ) as
    SolidityVector ns a -> expandVector ns a
    SolidityArray a     -> appT listT $ toHSType a
  where
    numLit n = litT (numTyLit $ toInteger n)
    expandVector :: [Int] -> SolidityType -> TypeQ
    expandVector ns a = case uncons ns of
      Just (n, rest) ->
        if null rest
          then conT ''ListN `appT` numLit n `appT` toHSType a
          else conT ''ListN `appT` numLit n `appT` expandVector rest a
      _ -> error $ "Impossible Nothing branch in `expandVector`: " ++ show ns ++ " " ++ show a

typeFuncQ :: FunctionArg -> TypeQ
typeFuncQ t = case parseSolidityFunctionArgType t of
  Left e   -> error $ "Unable to parse solidity type: " ++ show e
  Right ty -> toHSType ty

typeEventQ :: EventArg -> TypeQ
typeEventQ t = case parseSolidityEventArgType t of
  Left e   -> error $ "Unable to parse solidity type: " ++ show e
  Right ty -> toHSType ty


-- | Function argument to TH type
funBangType :: FunctionArg -> BangTypeQ
funBangType fa =
    bangType (bang sourceNoUnpack sourceStrict) (typeFuncQ fa)

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
           -> DecsQ
funWrapper c name dname args result = do
    vars <- replicateM (length args) (newName "t")
    a <- varT <$> newName "a"
    t <- varT <$> newName "t"
    m <- varT <$> newName "m"


    let params  = appsE $ conE dname : fmap varE vars
        inputT  = fmap typeFuncQ args
        outputT = case result of
            Nothing  -> [t|$t $m ()|]
            Just [x] -> [t|$t $m $(typeFuncQ x)|]
            Just xs  -> let outs = fmap typeFuncQ xs
                         in  [t|$t $m $(foldl appT (tupleT (length xs)) outs)|]

    sequence [
        sigD name $ [t|
            (JsonRpc $m, Account $a $t, Functor ($t $m)) =>
                $(arrowing $ inputT ++ [if c then outputT else [t|$t $m TxReceipt|]])
            |]
      , if c
            then funD' name (varP <$> vars) $ case result of
                    Just [_] -> [|only <$> call $(params)|]
                    _        -> [|call $(params)|]
            else funD' name (varP <$> vars) $ [|send $(params)|]
      ]
  where
    arrowing []       = error "Impossible branch call"
    arrowing [x]      = x
    arrowing (x : xs) = [t|$x -> $(arrowing xs)|]

mkDecl :: Declaration -> DecsQ

mkDecl ev@(DEvent uncheckedName inputs anonymous) = sequence
    [ dataD' indexedName (normalC indexedName (map (toBang <=< tag) indexedArgs)) derivingD
    , instanceD' indexedName (conT ''Generic) []
    , instanceD' indexedName (conT ''AbiType) [funD' 'isDynamic [] [|const False|]]
    , instanceD' indexedName (conT ''AbiGet) []
    , dataD' nonIndexedName (normalC nonIndexedName (map (toBang <=< tag) nonIndexedArgs)) derivingD
    , instanceD' nonIndexedName (conT ''Generic) []
    , instanceD' nonIndexedName (conT ''AbiType) [funD' 'isDynamic [] [|const False|]]
    , instanceD' nonIndexedName (conT ''AbiGet) []
    , dataD' allName (recC allName (map (\(n, a) -> (\(b,t) -> return (n,b,t)) <=< toBang <=< typeEventQ $ a) allArgs)) derivingD
    , instanceD' allName (conT ''Generic) []
    , instanceD (cxt [])
        (pure $ ConT ''IndexedEvent `AppT` ConT indexedName `AppT` ConT nonIndexedName `AppT` ConT allName)
        [funD' 'isAnonymous [] [|const anonymous|]]
    , instanceD (cxt [])
        (pure $ ConT ''Default `AppT` (ConT ''Filter `AppT` ConT allName))
        [funD' 'def [] [|Filter Nothing Latest Latest $ Just topics|] ]
    ]
  where
    name = if toLower (T.head uncheckedName) == Char.toUpper (T.head uncheckedName) then "EvT" <> uncheckedName else uncheckedName
    topics    = [Just (T.unpack $ eventId ev)] <> replicate (length indexedArgs) Nothing
    toBang ty = bangType (bang sourceNoUnpack sourceStrict) (return ty)
    tag (n, ty) = AppT (AppT (ConT ''Tagged) (LitT $ NumTyLit n)) <$> typeEventQ ty
    labeledArgs = zip [1..] inputs
    indexedArgs = map (\(n, ea) -> (n, ea)) . filter (eveArgIndexed . snd) $ labeledArgs
    indexedName = mkName $ over _head toUpper (T.unpack name) <> "Indexed"
    nonIndexedArgs = map (\(n, ea) -> (n, ea)) . filter (not . eveArgIndexed . snd) $ labeledArgs
    nonIndexedName = mkName $ over _head toUpper (T.unpack name) <> "NonIndexed"
    allArgs :: [(Name, EventArg)]
    allArgs = makeArgs name $ map (\i -> (eveArgName i, i)) inputs
    allName = mkName $ over _head toUpper (T.unpack name)
    derivingD = [''Show, ''Eq, ''Ord, ''GHC.Generic]

-- TODO change this type name also
-- | Method declarations maker
mkDecl fun@(DFunction name constant inputs outputs) = (++)
  <$> funWrapper constant fnName dataName inputs outputs
  <*> sequence
        [ dataD' dataName (normalC dataName bangInput) derivingD
        , instanceD' dataName (conT ''Generic) []
        , instanceD' dataName (conT ''AbiType)
          [funD' 'isDynamic [] [|const False|]]
        , instanceD' dataName (conT ''AbiPut) []
        , instanceD' dataName (conT ''AbiGet) []
        , instanceD' dataName (conT ''Method)
          [funD' 'selector [] [|const mIdent|]]
        ]
  where mIdent    = T.unpack (methodId $ fun {funName = T.replace "'" "" name})
        dataName  = mkName (over _head toUpper (T.unpack $ T.dropWhile (== '_') name <> "Data"))
        fnName    = mkName (over _head toLower (T.unpack name))
        bangInput = fmap funBangType inputs
        derivingD = [''Show, ''Eq, ''Ord, ''GHC.Generic]

mkDecl _ = return []

mkContractDecl :: Text -> Text -> Text -> Declaration -> DecsQ
mkContractDecl name a b (DConstructor inputs) = sequence
    [ dataD' dataName (normalC dataName bangInput) derivingD
    , instanceD' dataName (conT ''Generic) []
    , instanceD' dataName (conT ''AbiType)
        [funD' 'isDynamic [] [|const False|]]
    , instanceD' dataName (conT ''AbiPut) []
    , instanceD' dataName (conT ''Method)
        [funD' 'selector [] [|convert . Contract.bytecode|]]
    , instanceD' dataName (conT ''Contract.Contract)
        [ funD' 'Contract.abi [] [|const abiString|]
        , funD' 'Contract.bytecode [] [|const bytecodeString|]
        ]
    ]
  where abiString = T.unpack a
        bytecodeString = T.unpack b
        dataName = mkName (over _head toUpper (T.unpack $ name <> "Contract"))
        bangInput = fmap funBangType inputs
        derivingD = [''Show, ''Eq, ''Ord, ''GHC.Generic]

mkContractDecl _ _ _ _ = return []

-- | this function gives appropriate names for the accessors in the following way
-- | argName -> evArgName
-- | arg_name -> evArg_name
-- | _argName -> evArgName
-- | "" -> evi , for example Transfer(address, address uint256) ~> Transfer {transfer1 :: address, transfer2 :: address, transfer3 :: Integer}
makeArgs :: Text -> [(Text, EventArg)] -> [(Name, EventArg)]
makeArgs prefix ns = go 1 ns
  where
    prefixStr = over _head toLower . T.unpack $ prefix
    go :: Int -> [(Text, EventArg)] -> [(Name, EventArg)]
    go _ [] = []
    go i ((h, ty) : tail')
        | T.null h  = (mkName $ prefixStr ++ show i, ty) : go (i + 1) tail'
        | otherwise = (mkName . (++ "_") . (++) prefixStr . over _head toUpper . T.unpack $ h, ty) : go (i + 1) tail'

escape :: [Declaration] -> [Declaration]
escape = escapeEqualNames . fmap escapeReservedNames

escapeEqualNames :: [Declaration] -> [Declaration]
escapeEqualNames = concatMap go . group . sort
  where go []       = []
        go (x : xs) = x : zipWith appendToName xs hats
        hats = [T.replicate n "'" | n <- [1..]]
        appendToName d@(DFunction n _ _ _) a = d { funName = n <> a }
        appendToName d@(DEvent n _ _) a      = d { eveName = n <> a }
        appendToName d _                     = d

escapeReservedNames :: Declaration -> Declaration
escapeReservedNames d@(DFunction n _ _ _)
  | isKeyword n = d { funName = n <> "'" }
  | otherwise = d
escapeReservedNames d = d

isKeyword :: Text -> Bool
isKeyword = flip elem [ "as", "case", "of", "class"
                      , "data", "family", "instance"
                      , "default", "deriving", "do"
                      , "forall", "foreign", "hiding"
                      , "if", "then", "else", "import"
                      , "infix", "infixl", "infixr"
                      , "let", "in", "mdo", "module"
                      , "newtype", "proc", "qualified"
                      , "rec", "type", "where"
                      ]

constructorSpec :: String -> Maybe (Text, Text, Text, Declaration)
constructorSpec str = do
    name     <- str ^? key "contractName" . _String
    abiValue <- str ^? key "abi"
    bytecode <- str ^? key "bytecode" . _String
    decl     <- filter isContructor . unAbi <$> str ^? key "abi" . _JSON
    return (name, jsonEncode abiValue, bytecode, toConstructor decl)
  where
    jsonEncode = LT.toStrict . LT.decodeUtf8 . Aeson.encode
    isContructor (DConstructor _) = True
    isContructor _                = False
    toConstructor []  = DConstructor []
    toConstructor [a] = a
    toConstructor _   = error "Broken ABI: more that one constructor"

-- | Abi to declarations converter
quoteAbiDec :: String -> DecsQ
quoteAbiDec str =
    case str ^? _JSON <|> str ^? key "abi" . _JSON <|> str ^? key "compilerOutput" . key "abi" . _JSON of
        Nothing                 -> fail "Unable to decode contract ABI"
        Just (ContractAbi decs) -> do
            funEvDecs <- concat <$> mapM mkDecl (escape decs)
            case constructorSpec str of
                Nothing -> return funEvDecs
                Just (a, b, c, d) -> (funEvDecs ++) <$> mkContractDecl a b c d

-- | Abi information string
quoteAbiExp :: String -> ExpQ
quoteAbiExp str =
    case str ^? _JSON <|> str ^? key "abi" . _JSON of
        Nothing                -> fail "Unable to decode contract ABI"
        Just a@(ContractAbi _) -> stringE (show a)
