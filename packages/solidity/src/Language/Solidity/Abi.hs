{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell   #-}

-- |
-- Module      :  Data.Solidity.Abi.Json
-- Copyright   :  Aleksandr Krupenkin 2016-2021
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  noportable
--
-- JSON encoded contract ABI parsers.
--

module Language.Solidity.Abi
    (
    -- * Contract ABI declarations
      ContractAbi(..)
    , Declaration(..)
    , FunctionArg(..)
    , EventArg(..)
    , StateMutability(..)

    -- * Method/Event id encoder
    , signature
    , methodId
    , eventId

    -- * Solidity type parser
    , SolidityType(..)
    , parseSolidityFunctionArgType
    , parseSolidityEventArgType
    ) where

import           Control.Monad            (void)
import           Crypto.Ethereum.Utils    (keccak256)
import           Data.Aeson               (FromJSON (parseJSON), Options (constructorTagModifier, fieldLabelModifier, sumEncoding),
                                           SumEncoding (TaggedObject),
                                           ToJSON (toJSON), defaultOptions, withObject, (.:), (.:?))
import           Data.Aeson.TH            (deriveJSON, deriveToJSON)
import qualified Data.ByteArray           as A (take)
import           Data.ByteArray.HexString (toText)
import           Data.Char                (toLower)
import           Data.Text                (Text)
import qualified Data.Text                as T (dropEnd, unlines, unpack)
import           Data.Text.Encoding       (encodeUtf8)
import           Lens.Micro               (over, _head)
import           Text.Parsec              (ParseError, char, choice, digit, eof,
                                           lookAhead, many1, manyTill,
                                           optionMaybe, parse, string, try,
                                           (<|>))
import           Text.Parsec.Text         (Parser)

-- | Method argument
data FunctionArg = FunctionArg
    { funArgName       :: Text
    -- ^ Argument name
    , funArgType       :: Text
    -- ^ Argument type
    , funArgComponents :: Maybe [FunctionArg]
    -- ^ Argument components for tuples
    }
    deriving (Show, Eq, Ord)

$(deriveJSON
    (defaultOptions {fieldLabelModifier = over _head toLower . drop 6})
    ''FunctionArg)

-- | Event argument
data EventArg = EventArg
    { eveArgName    :: Text
    -- ^ Argument name
    , eveArgType    :: Text
    -- ^ Argument type
    , eveArgIndexed :: Bool
    -- ^ Argument is indexed (e.g. placed on topics of event)
    }
    deriving (Show, Eq, Ord)

$(deriveJSON
    (defaultOptions {fieldLabelModifier = over _head toLower . drop 6})
    ''EventArg)

data StateMutability
  = SMPure
  | SMView
  | SMPayable
  | SMNonPayable
  deriving (Eq, Ord, Show)

$(deriveJSON (defaultOptions {
    sumEncoding = TaggedObject "stateMutability" "contents"
  , constructorTagModifier = fmap toLower . drop 2 })
    ''StateMutability)

-- | Elementary contract interface item
data Declaration = DConstructor
    { conInputs :: [FunctionArg]
    -- ^ Contract constructor
    }
    | DFunction
    { funName     :: Text
    , funConstant :: Bool
    , funInputs   :: [FunctionArg]
    , funOutputs  :: Maybe [FunctionArg]
    -- ^ Method
    }
    | DEvent
    { eveName      :: Text
    , eveInputs    :: [EventArg]
    , eveAnonymous :: Bool
    -- ^ Event
    }
    | DError
    { errName      :: Text
    , errInputs    :: [FunctionArg]
    -- ^ Error
    }
    | DFallback
    { falPayable :: Bool
    -- ^ Fallback function
    }
    deriving Show

instance Eq Declaration where
    (DConstructor a) == (DConstructor b) = length a == length b
    (DFunction a _ _ _) == (DFunction b _ _ _) = a == b
    (DEvent a _ _) == (DEvent b _ _) = a == b
    (DError a _) == (DError b _) = a == b
    (DFallback _) == (DFallback _) = True
    (==) _ _ = False

instance Ord Declaration where
    compare (DConstructor a) (DConstructor b) = compare (length a) (length b)
    compare (DFunction a _ _ _) (DFunction b _ _ _) = compare a b
    compare (DEvent a _ _) (DEvent b _ _) = compare a b
    compare (DError a _) (DError b _) = compare a b
    compare (DFallback _) (DFallback _) = EQ

    compare DConstructor {} DFunction {} = LT
    compare DConstructor {} DEvent {} = LT
    compare DConstructor {} DError {} = LT
    compare DConstructor {} DFallback {} = LT

    compare DFunction {} DConstructor {} = GT
    compare DFunction {} DEvent {} = LT
    compare DFunction {} DError {} = LT
    compare DFunction {} DFallback {} = LT

    compare DEvent {} DConstructor {} = GT
    compare DEvent {} DFunction {} = GT
    compare DEvent {} DError {} = LT
    compare DEvent {} DFallback {} = LT

    compare DError {} DConstructor {} = GT
    compare DError {} DFunction {} = GT
    compare DError {} DEvent {} = GT
    compare DError {} DFallback {} = LT

    compare DFallback {} DConstructor {} = GT
    compare DFallback {} DFunction {} = GT
    compare DFallback {} DEvent {} = GT
    compare DFallback {} DError {} = GT

instance FromJSON Declaration where
  parseJSON = withObject "Declaration" $ \o -> do
    t :: Text <- o .: "type"
    case t of
      "fallback" -> DFallback <$> o .: "payable"
      "constructor" -> DConstructor <$> o .: "inputs"
      "event" -> DEvent <$> o .: "name" <*> o .: "inputs" <*> o .: "anonymous"
      "error" -> DError <$> o .: "name" <*> o .: "inputs"
      "function" -> DFunction <$> o .: "name" <*> parseSm o <*> o .: "inputs" <*> o .:? "outputs"
      _ -> fail "value of 'type' not recognized"
      where
        parseSm o = do
          o .:? "stateMutability" >>= \case
            Nothing -> o .: "constant"
            Just sm -> pure $ sm `elem` [SMPure, SMView]

$(deriveToJSON
   (defaultOptions {
       sumEncoding = TaggedObject "type" "contents"
       , constructorTagModifier = over _head toLower . drop 1
       , fieldLabelModifier = over _head toLower . drop 3 })
   ''Declaration)

-- | Contract Abi is a list of method / event declarations
newtype ContractAbi = ContractAbi { unAbi :: [Declaration] }
  deriving (Eq, Ord)

instance FromJSON ContractAbi where
    parseJSON = fmap ContractAbi . parseJSON

instance ToJSON ContractAbi where
    toJSON = toJSON . unAbi

instance Show ContractAbi where
    show (ContractAbi c) = T.unpack $ T.unlines $
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

funArgs :: [FunctionArg] -> Text
funArgs [] = ""
funArgs [x] = funArgType x
funArgs (x:xs) = case funArgComponents x of
  Nothing   -> funArgType x <> "," <> funArgs xs
  Just cmps -> case funArgType x of
      "tuple" -> "(" <> funArgs cmps <> ")," <> funArgs xs
      "tuple[]" -> "(" <> funArgs cmps <> ")[]," <> funArgs xs
      typ       -> error $ "Unexpected type " ++ T.unpack typ ++ " - expected tuple or tuple[]"

-- | Take a signature by given decl, e.g. foo(uint,string)
signature :: Declaration -> Text

signature (DConstructor inputs) = "(" <> funArgs inputs <> ")"
signature (DFallback _) = "()"
signature (DFunction name _ inputs _) = name <> "(" <> funArgs inputs <> ")"
signature (DError name inputs) = name <> "(" <> funArgs inputs <> ")"
signature (DEvent name inputs _) = name <> "(" <> args inputs <> ")"
  where
    args :: [EventArg] -> Text
    args = T.dropEnd 1 . foldMap (<> ",") . fmap eveArgType

-- | Generate method selector by given method 'Delcaration'
methodId :: Declaration -> Text
{-# INLINE methodId #-}
methodId = toText . A.take 4 . keccak256 . encodeUtf8 . signature

-- | Generate event `topic0` hash by givent event 'Delcaration'
eventId :: Declaration -> Text
{-# INLINE eventId #-}
eventId = toText . keccak256 . encodeUtf8 . signature

-- | Solidity types and parsers
data SolidityType = SolidityBool
    | SolidityAddress
    | SolidityUint Int
    | SolidityInt Int
    | SolidityString
    | SolidityBytesN Int
    | SolidityBytes
    | SolidityTuple [SolidityType]
    | SolidityVector [Int] SolidityType
    | SolidityArray SolidityType
    deriving (Eq, Show)

numberParser :: Parser Int
numberParser = read <$> many1 digit

parseUint :: Parser SolidityType
parseUint = do
  _ <- string "uint"
  SolidityUint <$> numberParser

parseInt :: Parser SolidityType
parseInt = do
  _ <- string "int"
  SolidityInt <$> numberParser

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
    ns <- many1Till lengthParser (lookAhead (void $ string "[]") <|> eof)
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
  s <- try (parseVector <* string "[]") <|> (solidityBasicTypeParser <* string "[]")
  pure $ SolidityArray s


solidityTypeParser :: Parser SolidityType
solidityTypeParser =
    choice [ try parseArray
           , try parseVector
           , solidityBasicTypeParser
           ]

parseSolidityFunctionArgType :: FunctionArg -> Either ParseError SolidityType
parseSolidityFunctionArgType (FunctionArg _ typ mcmps) = case mcmps of
  Nothing -> parse solidityTypeParser "Solidity" typ
  Just cmps -> do
    tpl <- SolidityTuple <$> mapM parseSolidityFunctionArgType cmps
    case typ of
        "tuple"   -> return tpl
        "tuple[]" -> return $ SolidityArray tpl
        _         -> error $ "Unexpected type " ++ T.unpack typ ++ " - expected tuple or tuple[]"

parseSolidityEventArgType :: EventArg -> Either ParseError SolidityType
parseSolidityEventArgType (EventArg _ typ _) = parse solidityTypeParser "Solidity" typ
