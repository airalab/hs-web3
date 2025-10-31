{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}

module Crypto.Ethereum.Eip712Signature
  ( EIP712Name
  , BitWidth (..)
  , ByteWidth (..)
  , HasWidth (..)
  , EIP712FieldType (..)
  , EIP712TypedData (..)
  , EIP712Struct (..)
  , EIP712Field (..)
  , EIP712Types
  , signTypedData
  , signTypedData'
  , hashStruct
  , encodeType
  , encodeData
  , typedDataSignHash
  )
where

import           Basement.Types.Word256   (Word256(..))
import           Control.Monad            (when)
import           Crypto.Ecdsa.Signature   (pack, sign)
import           Crypto.Ethereum          (PrivateKey, keccak256)
import           Data.Aeson               (Object, (.=))
import qualified Data.Aeson               as Aeson
import           Data.Aeson.Key           (fromText)
import qualified Data.Aeson.KeyMap        as Aeson
import           Data.Aeson.Types         (object)
import           Data.ByteArray           (ByteArray, zero)
import qualified Data.ByteArray           as BA
import           Data.ByteArray.HexString (hexString)
import           Data.ByteString          (ByteString, toStrict)
import qualified Data.ByteString          as BS
import           Data.ByteString.Builder  (toLazyByteString, word64BE)
import           Data.Either              (partitionEithers)
import           Data.Foldable            (toList)
import           Data.List                (find, intercalate)
import           Data.Maybe               (mapMaybe)
import           Data.Scientific          (Scientific, floatingOrInteger)
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Data.String              (fromString)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Text.Encoding       (encodeUtf8)
import qualified Data.Text.Encoding       as TE
import           Data.Word                (Word8)
import           Numeric.Natural          (Natural)

type DefaultByteArray = ByteString

-- Bit and Byte width constants, used for defining field types

data BitWidth
  = Si8
  | Si16
  | Si24
  | Si32
  | Si40
  | Si48
  | Si56
  | Si64
  | Si72
  | Si80
  | Si88
  | Si96
  | Si104
  | Si112
  | Si120
  | Si128
  | Si136
  | Si144
  | Si152
  | Si160
  | Si168
  | Si176
  | Si184
  | Si192
  | Si200
  | Si208
  | Si216
  | Si224
  | Si232
  | Si240
  | Si248
  | Si256
  deriving (Show, Eq, Bounded, Enum)

data ByteWidth
  = S1
  | S2
  | S3
  | S4
  | S5
  | S6
  | S7
  | S8
  | S9
  | S10
  | S11
  | S12
  | S13
  | S14
  | S15
  | S16
  | S17
  | S18
  | S19
  | S20
  | S21
  | S22
  | S23
  | S24
  | S25
  | S26
  | S27
  | S28
  | S29
  | S30
  | S31
  | S32
  deriving (Show, Eq, Bounded, Enum)

class HasWidth a where
  bytesOf :: a -> Int

instance HasWidth ByteWidth where
  bytesOf a = fromEnum a + 1

instance HasWidth BitWidth where
  bytesOf a = fromEnum a + 1

bitsOf :: (HasWidth a) => a -> Int
bitsOf a = bytesOf a * 8

-- EIP712 Data structures

type EIP712Name = Text

data EIP712FieldType
  = FieldTypeBytesN ByteWidth
  | FieldTypeUInt BitWidth
  | FieldTypeInt BitWidth
  | FieldTypeBool
  | FieldTypeAddress
  | FieldTypeBytes
  | FieldTypeString
  | FieldTypeArrayN Natural EIP712FieldType
  | FieldTypeArray EIP712FieldType
  | FieldTypeStruct EIP712Name
  deriving (Show, Eq)

data EIP712Field = EIP712Field
  { eip712FieldName :: EIP712Name
  , eip712FieldType :: EIP712FieldType
  }
  deriving (Show, Eq)

data EIP712Struct = EIP712Struct
  { eip712StructName   :: EIP712Name
  , eip712StructFields :: [EIP712Field]
  }
  deriving (Show, Eq)

type EIP712Types = [EIP712Struct]

data EIP712TypedData
  = EIP712TypedData
  { typedDataTypes       :: EIP712Types
  , typedDataPrimaryType :: EIP712Name
  , typedDataDomain      :: Object
  , typedDataMessage     :: Object
  }
  deriving (Show)

-- ToJSON serialization

instance Aeson.ToJSON EIP712Field where
  toJSON field = object ["name" .= eip712FieldName field, "type" .= TE.decodeUtf8 (encode $ eip712FieldType field)]

instance Aeson.ToJSON EIP712TypedData where
  toJSON typedData =
    object
      [ "types"
          .= object
            [ fromText (eip712StructName s) .= eip712StructFields s
            | s <- typedDataTypes typedData
            ]
      , "primaryType" .= typedDataPrimaryType typedData
      , "domain" .= typedDataDomain typedData
      , "message" .= typedDataMessage typedData
      ]

-- Custom EIP712 encoding

class EIP712Encoded a where
  encode :: (ByteArray bout) => a -> bout -- TODO Check if using ByteArray is better

instance EIP712Encoded EIP712FieldType where
  encode = \case
    FieldTypeBytesN sb -> utf8 "bytes" <> toByteArray (bytesOf sb)
    FieldTypeUInt sb -> utf8 "uint" <> toByteArray (bitsOf sb)
    FieldTypeInt sb -> utf8 "int" <> toByteArray (bitsOf sb)
    FieldTypeBool -> utf8 "bool"
    FieldTypeAddress -> utf8 "address"
    FieldTypeBytes -> utf8 "bytes"
    FieldTypeString -> utf8 "string"
    FieldTypeArrayN n t -> encode t <> utf8 "[" <> toByteArray n <> utf8 "]"
    FieldTypeArray t -> encode t <> utf8 "[]"
    FieldTypeStruct name -> utf8 name

instance EIP712Encoded EIP712Field where
  encode EIP712Field{..} = encode eip712FieldType <> utf8 " " <> utf8 eip712FieldName

-- | Encode a type according to the EIP712 specification (see Definition of `encodeType`)
encodeType :: (ByteArray bout) => EIP712Types -> EIP712Name -> Either String bout
encodeType types typeName = do
  struct <- lookupType types typeName
  refs <- referencedTypesEncoded struct
  let base = encodeUtf8 typeName <> "(" <> fieldsEncoded struct <> ")"
  pure $ BA.convert $ base <> refs
  where
    fieldsEncoded :: EIP712Struct -> BS.ByteString
    fieldsEncoded = BS.intercalate "," . fmap encode . eip712StructFields

    referencedTypesEncoded :: EIP712Struct -> Either String ByteString
    referencedTypesEncoded =
      fmap (BS.concat . toList)
        . traverse (encodeType types)
        . Set.toList
        . referencedTypesNames

    referencedTypesNames :: EIP712Struct -> Set EIP712Name
    referencedTypesNames = Set.fromList . mapMaybe (maybeReferenceTypeName . eip712FieldType) . eip712StructFields

    maybeReferenceTypeName :: EIP712FieldType -> Maybe EIP712Name
    maybeReferenceTypeName = \case
      FieldTypeArray inner -> maybeReferenceTypeName inner
      FieldTypeArrayN _ inner -> maybeReferenceTypeName inner
      FieldTypeStruct name -> Just name
      _ -> Nothing

-- | Encode data according to the EIP712 specification (see Definition of `encodeData`)
encodeData :: (ByteArray bout) => EIP712Types -> EIP712Name -> Aeson.Object -> Either String bout
encodeData types typeName obj = do
  encodedFields <- fieldsAndValues >>= mapM (uncurry encodeValue)
  return $ BA.concat encodedFields
  where
    findValue :: Text -> Either String Aeson.Value
    findValue fieldName = case Aeson.lookup (fromString $ T.unpack fieldName) obj of
      Just v  -> Right v
      Nothing -> Left $ fromString $ T.unpack fieldName

    fieldsAndValues :: Either String [(EIP712FieldType, Aeson.Value)]
    fieldsAndValues = do
      fields <- eip712StructFields <$> lookupType types typeName
      let valueOrFieldNameList = fmap (findValue . eip712FieldName) fields
      let (missingFields, values) = partitionEithers valueOrFieldNameList
      if (not . null) missingFields
        then Left $ "missing fields" <> intercalate ", " missingFields
        else Right $ zip (fmap eip712FieldType fields) values

    encodeValue :: EIP712FieldType -> Aeson.Value -> Either String BA.Bytes
    encodeValue (FieldTypeBytesN s) v = do
      encodedBytes <- extractString v >>= hexString . encodeUtf8
      when (BA.length encodedBytes /= bytesOf s) $ Left $ "expected" <> show (bytesOf s) <>  "bytes, got " <> show (BA.length encodedBytes)
      return $ BA.convert encodedBytes <> zero (32 - bytesOf s)
    encodeValue (FieldTypeUInt _) v = do
      value <- extractNumber v >>= scientificToWord256
      when (value < 0) $ Left $ "expected unsigned int, got negative value " <> show value
      return $ encodeWord256 value
    encodeValue (FieldTypeInt _) v = encodeWord256 <$> (extractNumber v >>= scientificToWord256)
    encodeValue FieldTypeBool v = encodeWord256 . fromIntegral . fromEnum <$> extractBool v
    encodeValue FieldTypeAddress v = do
      valueAsHexString <- extractString v >>= hexString . encodeUtf8
      when (BA.length valueAsHexString /= 20) $ Left ("address not valid:" <> show v)
      return $ BA.convert $ zero 12 <> valueAsHexString
    encodeValue FieldTypeBytes v = do
      valueAsHexString <- extractString v >>= hexString . encodeUtf8
      return $ keccak256 valueAsHexString
    encodeValue FieldTypeString v = keccak256 . encodeUtf8 <$> extractString v
    encodeValue (FieldTypeArrayN _ innerType) v = encodeArray innerType v
    encodeValue (FieldTypeArray innerType) v = encodeArray innerType v
    encodeValue (FieldTypeStruct innerTypeName) v = do
      valueAsObject <- extractObject v
      hashStruct types innerTypeName valueAsObject

    encodeArray innerType v = do
      valueAsArray <- extractArray v
      encodedValues <- traverse (encodeValue innerType) valueAsArray
      return $ keccak256 $ BA.concat @BA.Bytes @BA.Bytes $ toList encodedValues

    encodeWord256 (Word256 a3 a2 a1 a0) = BA.convert $ toStrict $ toLazyByteString $ word64BE a3 <> word64BE a2 <> word64BE a1 <> word64BE a0


-- | Compute a hash for the struct according to EIP712 (see Definition of `hashStruct`)
hashStruct :: (ByteArray bout) => EIP712Types -> EIP712Name -> Aeson.Object -> Either String bout
hashStruct types typeName obj = do
  encodedData <- encodeData @DefaultByteArray types typeName obj
  encodedType <- encodeType @DefaultByteArray types typeName
  let typeHash = keccak256 encodedType
  return $ keccak256 $ typeHash <> encodedData

-- | Sign a EIP712 type data, returns encoded version of the signature
signTypedData :: (ByteArray rsv) => PrivateKey -> EIP712TypedData -> Either String rsv
signTypedData key typedData = pack <$> signTypedData' key typedData

-- | Sign a EIP712 type data, returns (r, s, v)
signTypedData' :: PrivateKey -> EIP712TypedData -> Either String (Integer, Integer, Word8)
signTypedData' key typedData = sign  @DefaultByteArray key <$> typedDataSignHash typedData

-- | Returns the hash that needs to be signed by the private key
typedDataSignHash ::  (ByteArray bout) =>  EIP712TypedData -> Either String bout
typedDataSignHash typedData = do
  domainSeparator <- hashStruct (typedDataTypes typedData) "EIP712Domain" (typedDataDomain typedData)
  hashStructMessage <- hashStruct (typedDataTypes typedData) (typedDataPrimaryType typedData) (typedDataMessage typedData)
  return $ keccak256 @DefaultByteArray (BA.pack [0x19, 0x01] <> domainSeparator <> hashStructMessage)

--------------------------------------------------------------------------------------------------------
-- Utility functions for data manipulation
--------------------------------------------------------------------------------------------------------

lookupType :: EIP712Types -> EIP712Name -> Either String EIP712Struct
lookupType types typeName =
  case find ((== typeName) . eip712StructName) types of
    Just struct -> Right struct
    Nothing ->
      Left $ "EIP712 type not found: " <> show typeName

describeJsonType :: Aeson.Value -> String
describeJsonType (Aeson.String _) = "string"
describeJsonType (Aeson.Number _) = "number"
describeJsonType (Aeson.Bool _)   = "boolean"
describeJsonType (Aeson.Array _)  = "array"
describeJsonType (Aeson.Object _) = "object"
describeJsonType Aeson.Null       = "null"

extractError :: String -> Aeson.Value -> Either String b
extractError expected v = Left $ "expected " <> expected <> ", got " <> describeJsonType v

extractString :: Aeson.Value -> Either String Text
extractString (Aeson.String v) = Right v
extractString v                = extractError "string" v

extractNumber :: Aeson.Value -> Either String Scientific
extractNumber (Aeson.Number v) = Right v
extractNumber v                = extractError "number" v

extractBool :: Aeson.Value -> Either String Bool
extractBool (Aeson.Bool v) = Right v
extractBool v              = extractError "bool" v

extractArray :: Aeson.Value -> Either String Aeson.Array
extractArray (Aeson.Array v) = Right v
extractArray v               = extractError "array" v

extractObject :: Aeson.Value -> Either String Aeson.Object
extractObject (Aeson.Object v) = Right v
extractObject v                = extractError "object" v

scientificToWord256 :: Scientific -> Either String Word256
scientificToWord256 n = case floatingOrInteger @Double n of
  Right r -> Right $ fromInteger r
  Left r  -> Left $ "Number is not an integer: " <> show r

--------------------------------------------------------------------------------------------------------
-- Utility functions for encoding
--------------------------------------------------------------------------------------------------------

-- | Generic "to UTF8 bytes" helper
utf8 :: (BA.ByteArray b) => T.Text -> b
utf8 = BA.convert . TE.encodeUtf8

-- | Convert a Show-able value to UTF8 bytes
toByteArray :: (Show a, BA.ByteArray b) => a -> b
toByteArray = utf8 . T.pack . show
