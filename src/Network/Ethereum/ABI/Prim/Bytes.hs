{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- |
-- Module      :  Network.Ethereum.ABI.Prim.Bytes
-- Copyright   :  Alexander Krupenkin 2016-2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  noportable
--
-- Ethereum ABI bytes and bytesN types.
--

module Network.Ethereum.ABI.Prim.Bytes (
    Bytes
  , BytesN
  ) where

import           Data.Aeson                    (FromJSON (..), ToJSON (..),
                                                Value (String))
import           Data.ByteArray                (Bytes, convert, length, zero)
import           Data.ByteArray.Encoding       (Base (Base16), convertFromBase,
                                                convertToBase)
import           Data.ByteArray.Sized          (SizedByteArray,
                                                unsafeFromByteArrayAccess)
import qualified Data.ByteArray.Sized          as S (take)
import           Data.ByteString               (ByteString)
import qualified Data.ByteString.Char8         as C8
import           Data.Monoid                   ((<>))
import           Data.Proxy                    (Proxy (..))
import           Data.Serialize                (Get, Putter, getBytes,
                                                putByteString)
import           Data.String                   (IsString (..))
import qualified Data.Text                     as T (append, drop, take)
import           Data.Text.Encoding            (decodeUtf8, encodeUtf8)
import           GHC.TypeLits
import           Prelude                       hiding (length)

import           Network.Ethereum.ABI.Class    (ABIGet (..), ABIPut (..),
                                                ABIType (..))
import           Network.Ethereum.ABI.Prim.Int (getWord256, putWord256)

instance ABIType ByteString where
    isDynamic _ = True

instance ABIGet ByteString where
    abiGet = abiGetByteString

instance ABIPut ByteString where
    abiPut = abiPutByteString

instance ABIType Bytes where
    isDynamic _ = True

instance ABIGet Bytes where
    abiGet = convert <$> abiGetByteString

instance ABIPut Bytes where
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

type BytesN n = SizedByteArray n Bytes

instance (n <= 32) => ABIType (BytesN n) where
    isDynamic _ = False

instance (KnownNat n, n <= 32) => ABIGet (BytesN n) where
    abiGet = do
        ba <- unsafeFromByteArrayAccess <$> getBytes 32
        return $ S.take (ba :: BytesN 32)

instance (KnownNat n, n <= 32) => ABIPut (BytesN n) where
    abiPut ba = putByteString $ convert ba <> zero (32 - len)
      where len = fromIntegral $ natVal (Proxy :: Proxy n)

instance (KnownNat n, n <= 32) => IsString (BytesN n) where
    fromString = unsafeFromByteArrayAccess . (fromString :: String -> Bytes)

abiGetByteString :: Get ByteString
abiGetByteString = do
    len <- fromIntegral <$> getWord256
    ba <- getBytes len
    _ <- getBytes $ 32 - len `mod` 32
    return ba

abiPutByteString :: Putter ByteString
abiPutByteString bs = do
    putWord256 $ fromIntegral len
    putByteString $ bs <> zero (32 - len `mod` 32)
  where len = length bs
