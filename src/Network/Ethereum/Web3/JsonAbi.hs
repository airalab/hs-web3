{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
-- |
-- Module      :  Network.Ethereum.Web3.JsonAbi
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  POSIX / WIN32
--
-- Ethereum smart contract JSON ABI parser.
--
module Network.Ethereum.Web3.JsonAbi (
    ContractABI(..)
  , Declaration(..)
  , FunctionArg(..)
  , EventArg(..)
  , signature
  , methodId
  , eventId
  ) where

import Crypto.Hash (Digest, Keccak_256, hash)
import qualified Data.Text.Encoding as T
import qualified Data.Text          as T
import Network.Ethereum.Web3.Internal
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Aeson.TH
import Data.Aeson

data FunctionArg = FunctionArg
  { funArgName :: Text
  , funArgType :: Text
  } deriving (Show, Eq, Ord)

$(deriveJSON
    (defaultOptions {fieldLabelModifier = toLowerFirst . drop 6})
    ''FunctionArg)

data EventArg = EventArg
  { eveArgName    :: Text
  , eveArgType    :: Text
  , eveArgIndexed :: Bool
  } deriving (Show, Eq, Ord)

$(deriveJSON
    (defaultOptions {fieldLabelModifier = toLowerFirst . drop 6})
    ''EventArg)

data Declaration
  = DConstructor { conInputs :: [FunctionArg] }
  | DFunction { funName      :: Text
              , funConstant  :: Bool
              , funInputs    :: [FunctionArg]
              , funOutputs   :: Maybe [FunctionArg] }
  | DEvent { eveName      :: Text
           , eveInputs    :: [EventArg]
           , eveAnonymous :: Bool }
  | DFallback { falPayable :: Bool }
  deriving (Show, Eq, Ord)

$(deriveJSON (defaultOptions {
    sumEncoding = defaultTaggedObject { tagFieldName = "type" }
  , constructorTagModifier = toLowerFirst . drop 1
  , fieldLabelModifier = toLowerFirst . drop 3 })
    ''Declaration)

newtype ContractABI = ContractABI { unABI :: [Declaration] }
  deriving (Eq, Ord)

instance Show ContractABI where
    show (ContractABI c) = T.unpack $ T.unlines $
        [ "Contract:" ]
        ++ foldMap showConstructor c ++
        [ "\tEvents:" ]
        ++ foldMap showEvent c ++
        [ "\tMethods:" ]
        ++ foldMap showMethod c

instance FromJSON ContractABI where
    parseJSON = fmap ContractABI . parseJSON

instance ToJSON ContractABI where
    toJSON (ContractABI x) = toJSON x

showConstructor :: Declaration -> [Text]
showConstructor x = case x of
    DConstructor{} -> ["\tConstructor " <> signature x]
    _ -> []

showEvent :: Declaration -> [Text]
showEvent x = case x of
    DEvent{} -> ["\t\t" <> signature x]
    _ -> []

showMethod :: Declaration -> [Text]
showMethod x = case x of
    DFunction{} ->
        ["\t\t" <> methodId x <> " " <> signature x]
    _ -> []

-- | Take a signature by given decl, e.g. foo(uint,string)
signature :: Declaration -> Text

signature (DConstructor inputs) = "(" <> args inputs <> ")"
  where args = T.dropEnd 1 . foldMap (<> ",") . fmap funArgType

signature (DFallback _) = "()"

signature (DFunction name _ inputs _) = name <> "(" <> args inputs <> ")"
  where args = T.dropEnd 1 . foldMap (<> ",") . fmap funArgType

signature (DEvent name inputs _) = name <> "(" <> args inputs <> ")"
  where args = T.dropEnd 1 . foldMap (<> ",") . fmap eveArgType

sha3 :: Text -> Text
{-# INLINE sha3 #-}
sha3 x = T.pack (show digest)
  where digest :: Digest Keccak_256
        digest = hash (T.encodeUtf8 x)

methodId :: Declaration -> Text
{-# INLINE methodId #-}
methodId = ("0x" <>) . T.take 8 . sha3 . signature

eventId :: Declaration -> Text
{-# INLINE eventId #-}
eventId = ("0x" <>) . sha3 . signature
