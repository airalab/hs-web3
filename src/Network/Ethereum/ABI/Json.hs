{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- |
-- Module      :  Network.Ethereum.ABI.Json
-- Copyright   :  Alexander Krupenkin 2016-2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  noportable
--
-- JSON encoded contract ABI parsers.
--

module Network.Ethereum.ABI.Json (
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

import           Control.Monad      (void)
import           Crypto.Hash        (Digest, Keccak_256, hash)
import           Data.Aeson         (FromJSON (parseJSON), Options (constructorTagModifier, fieldLabelModifier, sumEncoding),
                                     SumEncoding (TaggedObject),
                                     ToJSON (toJSON), defaultOptions)
import           Data.Aeson.TH      (deriveJSON)
import           Data.Monoid        ((<>))
import           Data.Text          (Text)
import qualified Data.Text          as T (dropEnd, pack, take, unlines, unpack)
import           Data.Text.Encoding (encodeUtf8)
import           Text.Parsec        (ParseError, char, choice, digit, eof,
                                     lookAhead, many1, manyTill, optionMaybe,
                                     parse, string, try, (<|>))
import           Text.Parsec.Text   (Parser)

import           Data.String.Extra  (toLowerFirst)

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
  deriving Show

instance Eq Declaration where
    (DConstructor a) == (DConstructor b) = length a == length b
    (DFunction a _ _ _) == (DFunction b _ _ _) = a == b
    (DEvent a _ _) == (DEvent b _ _) = a == b
    (DFallback _) == (DFallback _) = True
    (==) _ _ = False

instance Ord Declaration where
    compare (DConstructor a) (DConstructor b) = compare (length a) (length b)
    compare (DFunction a _ _ _) (DFunction b _ _ _) = compare a b
    compare (DEvent a _ _) (DEvent b _ _) = compare a b
    compare (DFallback _) (DFallback _) = EQ

    compare DConstructor {} DFunction {} = LT
    compare DConstructor {} DEvent {} = LT
    compare DConstructor {} DFallback {} = LT

    compare DFunction {} DConstructor {} = GT
    compare DFunction {} DEvent {} = LT
    compare DFunction {} DFallback {} = LT

    compare DEvent {} DConstructor {} = GT
    compare DEvent {} DFunction {} = GT
    compare DEvent {} DFallback {} = LT

    compare DFallback {} DConstructor {} = GT
    compare DFallback {} DFunction {} = GT
    compare DFallback {} DEvent {} = GT

$(deriveJSON (defaultOptions {
    sumEncoding = TaggedObject "type" "contents"
  , constructorTagModifier = toLowerFirst . drop 1
  , fieldLabelModifier = toLowerFirst . drop 3 })
    ''Declaration)

-- | Contract ABI is a list of method / event declarations
newtype ContractABI = ContractABI { unABI :: [Declaration] }
  deriving (Eq, Ord)

instance FromJSON ContractABI where
    parseJSON = fmap ContractABI . parseJSON

instance ToJSON ContractABI where
    toJSON = toJSON . unABI

instance Show ContractABI where
    show (ContractABI c) = T.unpack $ T.unlines $
        [ "Contract:" ]
        ++ foldMap showConstructor c ++
        [ "\tEvents:" ]
        ++ foldMap showEvent c ++
        [ "\tMethods:" ]
        ++ foldMap showMethod c

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
  where
    args :: [FunctionArg] -> Text
    args = T.dropEnd 1 . foldMap (<> ",") . fmap funArgType

signature (DFallback _) = "()"

signature (DFunction name _ inputs _) = name <> "(" <> args inputs <> ")"
  where
    args :: [FunctionArg] -> Text
    args = T.dropEnd 1 . foldMap (<> ",") . fmap funArgType

signature (DEvent name inputs _) = name <> "(" <> args inputs <> ")"
  where
    args :: [EventArg] -> Text
    args = T.dropEnd 1 . foldMap (<> ",") . fmap eveArgType

-- | Localy compute Keccak-256 hash of given text
sha3 :: Text -> Text
{-# INLINE sha3 #-}
sha3 x = T.pack (show digest)
  where digest :: Digest Keccak_256
        digest = hash (encodeUtf8 x)

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
  | SolidityBytes
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
  pure $ maybe SolidityBytes SolidityBytesN mn

parseAddress :: Parser SolidityType
parseAddress = string "address" >> pure SolidityAddress

solidityBasicTypeParser :: Parser SolidityType
solidityBasicTypeParser =
    choice [ try parseUint
           , try parseInt
           , try parseAddress
           , try parseBool
           , try parseString
           , parseBytes
           ]

parseVector :: Parser SolidityType
parseVector = do
    s <- solidityBasicTypeParser
    ns <- many1Till lengthParser ((lookAhead $ void (string "[]")) <|> eof)
    pure $ SolidityVector ns s
  where
    many1Till :: Parser Int -> Parser () -> Parser [Int]
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
parseSolidityType = parse solidityTypeParser "Solidity"
