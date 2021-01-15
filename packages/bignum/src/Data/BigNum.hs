{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

-- |
-- Module      :  Data.BigNum
-- Copyright   :  Aleksandr Krupenkin 2016-2020
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- Big numbers and codecs for Haskell Web3 library.
--

module Data.BigNum (Word256, Word128, H256, h256) where

import           Basement.Block          (Block)
import           Basement.Types.Word128  (Word128 (..))
import           Basement.Types.Word256  (Word256 (..))
import           Codec.Scale             ()
import           Codec.Scale.Class       (Decode (..), Encode (..))
import           Data.ByteArray          (ByteArrayAccess, convert)
import qualified Data.ByteArray          as A (drop, length, take)
import           Data.ByteArray.Encoding (Base (Base16), convertFromBase,
                                          convertToBase)
import           Data.ByteString         (ByteString)
import           Data.Maybe              (fromJust)
import           Data.Serialize.Get      (getByteString)
import           Data.Serialize.Put      (putByteString)
import           Data.String             (IsString (..))
import           Data.Word               (Word8)

instance Encode Word128 where
    put (Word128 l h)= put h >> put l

instance Decode Word128 where
    get = flip Word128 <$> get <*> get

instance Encode Word256 where
    put (Word256 lx hx l h) = do
        put h
        put l
        put hx
        put lx

instance Decode Word256 where
    get = do
        h <- get
        l <- get
        hx <- get
        lx <- get
        return (Word256 lx hx l h)

-- | 32 byte of data.
newtype H256 = H256 { unH256 :: Block Word8 }
    deriving (Eq, Ord, ByteArrayAccess)

-- | Convert any 32 byte array into H256 type, otherwise returns Nothing.
h256 :: ByteArrayAccess a => a -> Maybe H256
h256 ba
  | A.length ba == 32 = Just $ H256 (convert ba)
  | otherwise = Nothing

fromHex :: ByteString -> Either String H256
fromHex bs | A.length bs' == 64 = H256 <$> convertFromBase Base16 bs'
           | otherwise = Left "wrong length"
  where
    hexStart = convert ("0x" :: ByteString)
    bs' | A.take 2 bs == hexStart = A.drop 2 bs
        | otherwise = bs

toHex :: Block Word8 -> ByteString
toHex = ("0x" <>) . convertToBase Base16

instance Show H256 where
    show = show . toHex . unH256

instance IsString H256 where
    fromString = either error id . fromHex . fromString

instance Encode H256 where
    put = putByteString . convert

instance Decode H256 where
    get = (fromJust . h256) <$> getByteString 32
