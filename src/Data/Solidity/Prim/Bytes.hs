{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- |
-- Module      :  Data.Solidity.Prim.Bytes
-- Copyright   :  Alexander Krupenkin 2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  noportable
--
-- Bytes and BytesN primitive types.
--

module Data.Solidity.Prim.Bytes
    (
    -- * The dynamic length @Bytes@ type
      Bytes

    -- * The fixed length @BytesN@ type
    , BytesN
    ) where

import           Control.Monad           (unless)
import           Data.Aeson              (FromJSON (..), ToJSON (..),
                                          Value (String))
import           Data.ByteArray          (Bytes, convert, length, zero)
import           Data.ByteArray.Encoding (Base (Base16), convertFromBase,
                                          convertToBase)
import           Data.ByteArray.Sized    (SizedByteArray, unSizedByteArray,
                                          unsafeFromByteArrayAccess)
import qualified Data.ByteArray.Sized    as S (take)
import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8   as C8
import           Data.Proxy              (Proxy (..))
import           Data.Serialize          (Get, Putter, getBytes, putByteString)
import           Data.String             (IsString (..))
import qualified Data.Text               as T (append, drop, take)
import           Data.Text.Encoding      (decodeUtf8, encodeUtf8)
import           GHC.TypeLits
import           Prelude                 hiding (length)

import           Data.Solidity.Abi       (AbiGet (..), AbiPut (..),
                                          AbiType (..))
import           Data.Solidity.Prim.Int  (getWord256, putWord256)

instance AbiType ByteString where
    isDynamic _ = True

instance AbiGet ByteString where
    abiGet = abiGetByteString

instance AbiPut ByteString where
    abiPut = abiPutByteString

instance AbiType Bytes where
    isDynamic _ = True

instance AbiGet Bytes where
    abiGet = convert <$> abiGetByteString

instance AbiPut Bytes where
    abiPut = abiPutByteString . convert

instance IsString Bytes where
    fromString ('0' : 'x' : hex) = either error id $ convertFromBase Base16 (C8.pack hex)
    fromString str               = convert (C8.pack str)

instance FromJSON Bytes where
    parseJSON (String hex)
        | T.take 2 hex == "0x" =
            either fail pure $ convertFromBase Base16 $ encodeUtf8 $ T.drop 2 hex
        | otherwise = fail "Hex string should have '0x' prefix"
    parseJSON _ = fail "Bytes should be encoded as hex string"

instance ToJSON Bytes where
    toJSON = toJSON . T.append "0x" . decodeUtf8 . convertToBase Base16

-- | Sized byte array with fixed length in bytes
type BytesN n = SizedByteArray n Bytes

instance (n <= 32) => AbiType (BytesN n) where
    isDynamic _ = False

instance (KnownNat n, n <= 32) => AbiGet (BytesN n) where
    abiGet = do
        ba <- unsafeFromByteArrayAccess <$> getBytes 32
        return $ S.take (ba :: BytesN 32)

instance (KnownNat n, n <= 32) => AbiPut (BytesN n) where
    abiPut ba = putByteString $ convert ba <> zero (32 - len)
      where len = fromIntegral $ natVal (Proxy :: Proxy n)

instance (KnownNat n, n <= 32) => IsString (BytesN n) where
    fromString s = unsafeFromByteArrayAccess padded
      where bytes = fromString s :: Bytes
            len = fromIntegral $ natVal (Proxy :: Proxy n)
            padded = bytes <> zero (len - length bytes)

instance (KnownNat n, n <= 32) => FromJSON (BytesN n) where
    parseJSON v = do ba <- parseJSON v
                     return $ unsafeFromByteArrayAccess (ba :: Bytes)

instance (KnownNat n, n <= 32) => ToJSON (BytesN n) where
    toJSON ba = toJSON (unSizedByteArray ba :: Bytes)

abiGetByteString :: Get ByteString
abiGetByteString = do
    len <- fromIntegral <$> getWord256
    if len == 0
        then return ""
        else getBytes len

abiPutByteString :: Putter ByteString
abiPutByteString bs = do
    putWord256 $ fromIntegral len
    unless (len == 0) $
      putByteString $ bs <> zero (32 - len `mod` 32)
  where len = length bs
