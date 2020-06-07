{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

-- |
-- Module      :  Data.ByteArray.HexString
-- Copyright   :  Alexander Krupenkin 2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- Hex string data type and useful functions.
--

module Data.ByteArray.HexString where

import           Data.Aeson              (FromJSON (..), ToJSON (..),
                                          Value (String), withText)
import           Data.ByteArray          (ByteArray, ByteArrayAccess, convert)
import qualified Data.ByteArray          as BA (drop, take)
import           Data.ByteArray.Encoding (Base (Base16), convertFromBase,
                                          convertToBase)
import           Data.ByteString         (ByteString)
import           Data.String             (IsString (..))
import           Data.Text               (Text)
import           Data.Text.Encoding      (decodeUtf8, encodeUtf8)

-- | Represents a Hex string. Guarantees that all characters it contains
--   are valid hex characters.
newtype HexString = HexString { unHexString :: ByteString }
  deriving (Eq, Ord, Semigroup, Monoid, ByteArrayAccess, ByteArray)

instance Show HexString where
    show = ("HexString " ++) . show . toText

instance IsString HexString where
    fromString = hexString' . fromString
      where
        hexString' :: ByteString -> HexString
        hexString' = either error id . hexString

instance FromJSON HexString where
    parseJSON = withText "HexString" $ either fail pure . hexString . encodeUtf8

instance ToJSON HexString where
    toJSON = String . toText

-- | Smart constructor which trims '0x' and validates length is even.
hexString :: ByteArray ba => ba -> Either String HexString
hexString bs = HexString <$> convertFromBase Base16 bs'
  where
    hexStart = convert ("0x" :: ByteString)
    bs' | BA.take 2 bs == hexStart = BA.drop 2 bs
        | otherwise = bs

-- | Reads a raw bytes and converts to hex representation.
fromBytes :: ByteArrayAccess ba => ba -> HexString
fromBytes = HexString . convert

-- | Access to the raw bytes of 'HexString'.
toBytes :: ByteArray ba => HexString -> ba
toBytes = convert . unHexString

-- | Access to a 'Text' representation of the 'HexString'
toText :: HexString -> Text
toText = ("0x" <>) . decodeUtf8 . convertToBase Base16 . unHexString
