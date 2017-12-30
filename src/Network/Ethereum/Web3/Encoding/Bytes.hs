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
  , BytesD(..)
  ) where

import           Data.ByteArray                          (Bytes)
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
newtype BytesN (n :: Nat) = BytesN { unBytesN :: Bytes }
  deriving (Eq, Ord)

update :: BytesN a -> Bytes -> BytesN a
{-# INLINE update #-}
update _ = BytesN

instance KnownNat n => EncodingType (BytesN n) where
    typeName  = const $ "bytes" <> (show . natVal $ (Proxy :: Proxy n))
    isDynamic = const False

instance KnownNat n => ABIEncode (BytesN n) where
    toDataBuilder (BytesN bytes) = bytesBuilder bytes

instance KnownNat n => ABIDecode (BytesN n) where
    fromDataParser = do
        let result   = undefined :: KnownNat n => BytesN n
            len      = fromIntegral (natVal result)
        bytesString <- T.take (len * 2) <$> takeHexChar 64
        return (update result (bytesDecode bytesString))

instance KnownNat n => Show (BytesN n) where
    show = show . BS16.encode . BA.convert . unBytesN

bytesBuilder :: Bytes -> B.Builder
{-# INLINE bytesBuilder #-}
bytesBuilder = alignL . B.fromText . T.decodeUtf8
             . BS16.encode . BA.convert

bytesDecode :: T.Text -> Bytes
{-# INLINE bytesDecode #-}
bytesDecode = BA.convert . fst . BS16.decode . T.encodeUtf8

-- | Dynamic length byte array
newtype BytesD = BytesD { unBytesD :: Bytes }
  deriving (Eq, Ord)

instance Monoid BytesD where
    mempty = BytesD mempty
    mappend (BytesD a) (BytesD b) = BytesD (mappend a b)

instance EncodingType BytesD where
    typeName  = const "bytes[]"
    isDynamic = const True

instance ABIEncode BytesD where
    toDataBuilder (BytesD bytes) = int256HexBuilder (BA.length bytes)
                                <> bytesBuilder bytes
instance ABIDecode BytesD where
    fromDataParser = do
        len <- int256HexParser
        if (len :: Integer) > fromIntegral (maxBound :: Int)
        then fail "Bytes length over bound!"
        else (BytesD . bytesDecode) <$> takeHexChar (fromIntegral len * 2)

instance Show BytesD where
    show = show . BS16.encode . BA.convert . unBytesD
