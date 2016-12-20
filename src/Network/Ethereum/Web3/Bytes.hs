{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds      #-}
-- |
-- Module      :  Network.Ethereum.Web3.Bytes
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  noportable
--
-- The type bytes<M> support.
--
module Network.Ethereum.Web3.Bytes (
    BytesN(..)
  , BytesD(..)
  ) where

import qualified Data.ByteString.Base16 as BS16 (decode, encode)
import qualified Data.Attoparsec.Text   as P
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Encoding     as T
import qualified Data.Text              as T
import qualified Data.ByteArray         as BA
import Network.Ethereum.Web3.EncodingUtils
import Network.Ethereum.Web3.Encoding
import GHC.TypeLits (KnownNat, Nat, natVal)
import Data.ByteArray (Bytes)
import Data.Monoid ((<>))

import Debug.Trace

-- | Fixed length byte array
newtype BytesN (n :: Nat) = BytesN { unBytesN :: Bytes }
  deriving (Eq, Ord)

update :: BytesN a -> Bytes -> BytesN a
update _ a = BytesN a

instance KnownNat n => ABIEncoding (BytesN n) where
    toDataBuilder (BytesN bytes) = bytesBuilder bytes
    fromDataParser = do
        let result   = undefined :: KnownNat n => BytesN n
            len      = fromIntegral (natVal result)
        bytesString <- T.take (len * 2) <$> P.take 64
        return (update result (bytesDecode bytesString))

instance KnownNat n => Show (BytesN n) where
    show = show . BS16.encode . BA.convert . unBytesN

bytesBuilder :: Bytes -> B.Builder
bytesBuilder = alignL . B.fromText . T.decodeUtf8
             . BS16.encode . BA.convert

bytesDecode :: T.Text -> Bytes
bytesDecode = BA.convert . fst . BS16.decode . T.encodeUtf8

-- | Dynamic length byte array
newtype BytesD = BytesD { unBytesD :: Bytes }
  deriving (Eq, Ord)

instance ABIEncoding BytesD where
    toDataBuilder (BytesD bytes) = int256HexBuilder (BA.length bytes)
                                <> bytesBuilder bytes
    fromDataParser = do
        len <- int256HexParser
        if (len :: Integer) > fromIntegral (maxBound :: Int)
        then fail "Bytes length over bound!"
        else (BytesD . bytesDecode) <$> P.take (fromIntegral len * 2)

instance Show BytesD where
    show = show . BS16.encode . BA.convert . unBytesD
