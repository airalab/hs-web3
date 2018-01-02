{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
-- |
-- Module      :  Network.Ethereum.Web3.JsonAbi
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Ethereum smart contract JSON ABI types.
--
module Network.Ethereum.Web3.JsonAbi (
    ContractABI(..)
  , Declaration(..)
  , FunctionArg(..)
  , EventArg(..)
  , signature
  , methodId
  , eventId
  , SolidityType(..)
  , parseSolidityType
  ) where

import           Control.Monad                  (void)
import           Crypto.Hash                    (Digest, Keccak_256, hash)
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Monoid                    ((<>))
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import           Network.Ethereum.Web3.Internal
import           Text.Parsec
import           Text.Parsec.Text

-- | Method argument
data FunctionArg = FunctionArg
  { funArgName :: Text
  -- ^ Argument name
  , funArgType :: Text
  -- ^ Argument type
  } deriving (Show, Eq, Ord)

$(deriveJSON
    (defaultOptions {fieldLabelModifier = toLowerFirst . drop 6})
    ''FunctionArg)

-- | Event argument
data EventArg = EventArg
  { eveArgName    :: Text
  -- ^ Argument name
  , eveArgType    :: Text
  -- ^ Argument type
  , eveArgIndexed :: Bool
  -- ^ Argument is indexed (e.g. placed on topics of event)
  } deriving (Show, Eq, Ord)

$(deriveJSON
    (defaultOptions {fieldLabelModifier = toLowerFirst . drop 6})
    ''EventArg)

-- | Elementrary contract interface item
data Declaration
  = DConstructor { conInputs :: [FunctionArg] }
  -- ^ Contract constructor
  | DFunction { funName     :: Text
              , funConstant :: Bool
              , funInputs   :: [FunctionArg]
              , funOutputs  :: Maybe [FunctionArg] }
  -- ^ Method
  | DEvent { eveName      :: Text
           , eveInputs    :: [EventArg]
           , eveAnonymous :: Bool }
  -- ^ Event
  | DFallback { falPayable :: Bool }
  -- ^ Fallback function
  deriving (Show, Eq, Ord)

$(deriveJSON (defaultOptions {
    sumEncoding = defaultTaggedObject { tagFieldName = "type" }
  , constructorTagModifier = toLowerFirst . drop 1
  , fieldLabelModifier = toLowerFirst . drop 3 })
    ''Declaration)

-- | Contract ABI is a list of method / event declarations
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
    _              -> []

showEvent :: Declaration -> [Text]
showEvent x = case x of
    DEvent{} -> ["\t\t" <> signature x]
    _        -> []

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

-- | Localy compute Keccak-256 hash of given text
sha3 :: Text -> Text
{-# INLINE sha3 #-}
sha3 x = T.pack (show digest)
  where digest :: Digest Keccak_256
        digest = hash (T.encodeUtf8 x)

-- | Generate method selector by given method 'Delcaration'
methodId :: Declaration -> Text
{-# INLINE methodId #-}
methodId = ("0x" <>) . T.take 8 . sha3 . signature

-- | Generate event `topic0` hash by givent event 'Delcaration'
eventId :: Declaration -> Text
{-# INLINE eventId #-}
eventId = ("0x" <>) . sha3 . signature

-- | Solidity types and parsers

data SolidityType =
    SolidityBool
  | SolidityAddress
  | SolidityUint Int
  | SolidityInt Int
  | SolidityString
  | SolidityBytesN Int
  | SolidityBytesD
  | SolidityVector [Int] SolidityType
  | SolidityArray SolidityType
    deriving (Eq, Show)

numberParser :: Parser Int
numberParser = read <$> many1 digit

parseUint :: Parser SolidityType
parseUint = do
  _ <- string "uint"
  n <- numberParser
  pure $ SolidityUint n

parseInt :: Parser SolidityType
parseInt = do
  _ <- string "int"
  n <- numberParser
  pure $ SolidityInt n

parseBool :: Parser SolidityType
parseBool = string "bool" >>  pure SolidityBool

parseString :: Parser SolidityType
parseString = string "string" >> pure SolidityString

parseBytes :: Parser SolidityType
parseBytes = do
  _ <- string "bytes"
  mn <- optionMaybe numberParser
  pure $ maybe SolidityBytesD SolidityBytesN  mn

parseAddress :: Parser SolidityType
parseAddress = string "address" >> pure SolidityAddress

solidityBasicTypeParser :: Parser SolidityType
solidityBasicTypeParser =
    choice [ parseUint
           , parseInt
           , parseAddress
           , parseBool
           , parseString
           , parseBytes
           ]

parseVector :: Parser SolidityType
parseVector = do
    s <- solidityBasicTypeParser
    ns <- many1Till lengthParser ((lookAhead $ void (string "[]")) <|> eof)
    pure $ SolidityVector ns s
  where
    many1Till p end = do
      a <- p
      as <- manyTill p end
      return (a : as)
    lengthParser = do
          _ <- char '['
          n <- numberParser
          _ <- char ']'
          pure n

parseArray :: Parser SolidityType
parseArray = do
  s <- (try $ parseVector <* string "[]") <|> (solidityBasicTypeParser <* string "[]")
  pure $ SolidityArray s


solidityTypeParser :: Parser SolidityType
solidityTypeParser =
    choice [ try parseArray
           , try parseVector
           , solidityBasicTypeParser
           ]

parseSolidityType :: Text -> Either ParseError SolidityType
parseSolidityType = parse solidityTypeParser ""
