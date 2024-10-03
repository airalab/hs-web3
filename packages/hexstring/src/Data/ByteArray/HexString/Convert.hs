{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Data.ByteArray.HexString.Convert
-- Copyright   :  Aleksandr Krupenkin 2016-2024
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- From/to hex conversion functions.
--

module Data.ByteArray.HexString.Convert where

import           Data.Aeson                        (FromJSON (..), ToJSON (..),
                                                    Value (String), withText)
import           Data.ByteArray                    (ByteArray, ByteArrayAccess,
                                                    convert)
import           Data.ByteArray.Encoding           (Base (Base16),
                                                    convertToBase)
import           Data.Text                         (Text)
import           Data.Text.Encoding                (decodeUtf8, encodeUtf8)

import           Data.ByteArray.HexString.Internal (HexString (..), hexString)

-- | Convert type into it's hex representation.
class ToHex a where
    toHex :: a -> HexString

-- | Convert hex string into a type or return an error.
class FromHex a where
    fromHex :: HexString -> Either String a

-- | Reads a raw bytes and converts to hex representation.
fromBytes :: ByteArrayAccess ba => ba -> HexString
fromBytes = HexString . convert

-- | Access to the raw bytes of 'HexString'.
toBytes :: ByteArray ba => HexString -> ba
toBytes = convert . unHexString

-- | Access to a 'Text' representation of the 'HexString'
toText :: HexString -> Text
toText = ("0x" <>) . decodeUtf8 . convertToBase Base16 . unHexString

instance FromJSON HexString where
    parseJSON = withText "HexString" $ either fail pure . hexString . encodeUtf8

instance ToJSON HexString where
    toJSON = String . toText
