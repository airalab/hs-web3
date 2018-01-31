{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      :  Network.Ethereum.Web3.Encoding.Bytes
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  noportable
--
-- The type bytes<M> support.
--
module Network.Ethereum.Web3.Encoding.Bytes (
    BytesN(..)
  , Bytes(..)
  ) where

import qualified Data.ByteArray                          as BA
import qualified Data.ByteString.Base16                  as BS16 (decode,
                                                                  encode)
import           Data.Monoid                             (Monoid (..), (<>))
import           Data.Proxy
import qualified Data.Text                               as T
import qualified Data.Text.Encoding                      as T
import qualified Data.Text.Lazy.Builder                  as B
import           GHC.TypeLits                            (KnownNat, Nat, natVal)

import           Network.Ethereum.Web3.Encoding
import           Network.Ethereum.Web3.Encoding.Internal

-- | Fixed length byte array
newtype BytesN (n :: Nat) = BytesN { unBytesN :: BA.Bytes }
  deriving (Eq, Ord)

instance KnownNat n => EncodingType (BytesN n) where
    typeName  = const $ "bytes" <> (show . natVal $ (Proxy :: Proxy n))
    isDynamic = const False

instance KnownNat n => ABIEncode (BytesN n) where
    toDataBuilder (BytesN bytes) = bytesBuilder bytes

instance KnownNat n => ABIDecode (BytesN n) where
    fromDataParser = do
        let result   = undefined :: KnownNat n => BytesN n
            len      = fromIntegral (natVal result)
            update :: BytesN a -> BA.Bytes -> BytesN a
            update _ = BytesN
        bytesString <- T.take (len * 2) <$> takeHexChar 64
        return $ update result (bytesDecode bytesString)

instance KnownNat n => Show (BytesN n) where
    show = show . BS16.encode . BA.convert . unBytesN

bytesBuilder :: BA.Bytes -> B.Builder
{-# INLINE bytesBuilder #-}
bytesBuilder = alignL . B.fromText . T.decodeUtf8
             . BS16.encode . BA.convert

bytesDecode :: T.Text -> BA.Bytes
{-# INLINE bytesDecode #-}
bytesDecode = BA.convert . fst . BS16.decode . T.encodeUtf8

-- | Dynamic length byte array
newtype Bytes = Bytes { unBytes :: BA.Bytes }
  deriving (Eq, Ord)

instance Monoid Bytes where
    mempty = Bytes mempty
    mappend (Bytes a) (Bytes b) = Bytes (mappend a b)

instance EncodingType Bytes where
    typeName  = const "bytes[]"
    isDynamic = const True

instance ABIEncode Bytes where
    toDataBuilder (Bytes bytes) = int256HexBuilder (BA.length bytes)
                               <> bytesBuilder bytes
instance ABIDecode Bytes where
    fromDataParser = do
        len <- int256HexParser
        if (len :: Integer) > fromIntegral (maxBound :: Int)
        then fail "Bytes length over bound!"
        else (Bytes . bytesDecode) <$> takeHexChar (fromIntegral len * 2)

instance Show Bytes where
    show = show . BS16.encode . BA.convert . unBytes
