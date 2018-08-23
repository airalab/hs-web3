{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Data.HexString ( HexString
                      , hexString
                      , fromBytes
                      , toBytes
                      , toText ) where

import           Data.Aeson              (FromJSON (..), ToJSON (..),
                                          Value (String), withText)
import           Data.ByteArray          (ByteArray, ByteArrayAccess, convert)
import           Data.ByteArray.Encoding (Base (Base16), convertFromBase,
                                          convertToBase)
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as BS (drop, take)
import           Data.Monoid             (Monoid, (<>))
import           Data.Semigroup          (Semigroup)
import           Data.String             (IsString (..))
import           Data.Text               (Text)
import qualified Data.Text.Encoding      as TE (decodeUtf8, encodeUtf8)

-- | Represents a Hex string. Guarantees that all characters it contains
--   are valid hex characters.
newtype HexString = HexString { unHexString :: ByteString }
  deriving (Eq, Ord, Semigroup, Monoid, ByteArrayAccess, ByteArray)

instance Show HexString where
    show = ("HexString " ++) . show . toText

instance IsString HexString where
    fromString = either error id . hexString . fromString

instance FromJSON HexString where
    parseJSON = withText "HexString" $ either fail pure . hexString . TE.encodeUtf8

instance ToJSON HexString where
    toJSON = String . toText

-- | Smart constructor which validates that all the text are actually
--   have `0x` prefix, hexadecimal characters and length is even.
hexString :: ByteString -> Either String HexString
hexString bs
  | BS.take 2 bs == "0x" = HexString <$> bs'
  | otherwise  = Left $ "Hex string should be '0x' prefixed: " ++ show bs
  where
    bs' = convertFromBase Base16 (BS.drop 2 bs)

-- | Reads a raw bytes and converts to hex representation.
fromBytes :: ByteArrayAccess ba => ba -> HexString
fromBytes = HexString . convert

-- | Access to the raw bytes of 'HexString'.
toBytes :: ByteArray ba => HexString -> ba
toBytes = convert . unHexString

-- | Access to a 'Text' representation of the 'HexString'
toText :: HexString -> Text
toText = ("0x" <>) . TE.decodeUtf8 . convertToBase Base16 . unHexString
