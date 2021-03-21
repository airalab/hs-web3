{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

-- |
-- Module      :  Data.ByteArray.HexString.Internal
-- Copyright   :  Aleksandr Krupenkin 2016-2021
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- Hex string data type.
--

module Data.ByteArray.HexString.Internal where

import           Codec.Scale             (Decode, Encode)
import           Data.ByteArray          (ByteArray, ByteArrayAccess, convert)
import qualified Data.ByteArray          as BA (drop, take)
import           Data.ByteArray.Encoding (Base (Base16), convertFromBase,
                                          convertToBase)
import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8   as C8 (unpack)
import           Data.String             (IsString (..))

-- | Represents a Hex string. Guarantees that all characters it contains
--   are valid hex characters.
newtype HexString = HexString { unHexString :: ByteString }
    deriving (Eq, Ord, Semigroup, Monoid, ByteArrayAccess, ByteArray, Encode, Decode)

instance Show HexString where
    show = ("0x" ++) . C8.unpack . unHexString'
      where
        unHexString' :: HexString -> ByteString
        unHexString' = convertToBase Base16 . unHexString

instance IsString HexString where
    fromString = hexString' . fromString
      where
        hexString' :: ByteString -> HexString
        hexString' = either error id . hexString

-- | Smart constructor which trims '0x' and validates length is even.
hexString :: ByteArray ba => ba -> Either String HexString
hexString bs = HexString <$> convertFromBase Base16 bs'
  where
    hexStart = convert ("0x" :: ByteString)
    bs' | BA.take 2 bs == hexStart = BA.drop 2 bs
        | otherwise = bs
