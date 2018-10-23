{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Data.Solidity.Prim.Address
-- Copyright   :  Alexander Krupenkin 2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  noportable
--
-- Ethreum account address.
--

module Data.Solidity.Prim.Address
    (
    -- * The @Address@ type
      Address

    -- * Hex string encoding
    , toHexString
    , fromHexString

    -- * Public key to @Address@ convertor
    , fromPubKey

    -- * EIP55 Mix-case checksum address encoding
    , toChecksum
    , verifyChecksum
    ) where

import           Control.Monad            ((<=<))
import           Crypto.Hash              (Keccak_256 (..), hashWith)
import           Crypto.Secp256k1         (PubKey, exportPubKey)
import           Data.Aeson               (FromJSON (..), ToJSON (..))
import           Data.Bits                ((.&.))
import           Data.Bool                (bool)
import           Data.ByteArray           (convert, zero)
import qualified Data.ByteArray           as BA (drop)
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS (take, unpack)
import qualified Data.ByteString.Char8    as C8 (drop, length, pack, unpack)
import qualified Data.Char                as C (toLower, toUpper)
import           Data.Default             (Default (..))
import           Data.String              (IsString (..))
import           Data.Text.Encoding       as T (encodeUtf8)
import           Generics.SOP             (Generic)
import qualified GHC.Generics             as GHC (Generic)

import           Data.ByteArray.HexString (HexString, fromBytes, toBytes,
                                           toText)
import           Data.Solidity.Abi        (AbiGet (..), AbiPut (..),
                                           AbiType (..))
import           Data.Solidity.Abi.Codec  (decode, encode)
import           Data.Solidity.Prim.Int   (UIntN)

-- | Ethereum account address
newtype Address = Address { unAddress :: UIntN 160 }
  deriving (Eq, Ord, GHC.Generic)

instance Generic Address

instance Default Address where
    def = Address 0

instance Show Address where
    show = show . toChecksum . T.encodeUtf8 . toText . toHexString

instance IsString Address where
    fromString = either error id . fromHexString . fromString

instance AbiType Address where
    isDynamic _ = False

instance AbiGet Address where
    abiGet = Address <$> abiGet

instance AbiPut Address where
    abiPut = abiPut . unAddress

instance FromJSON Address where
    parseJSON = (either fail pure . fromHexString) <=< parseJSON

instance ToJSON Address where
    toJSON = toJSON . toHexString

-- | Derive address from secp256k1 public key
fromPubKey :: PubKey -> Address
fromPubKey key =
    case decode $ zero 12 <> BA.drop 12 (sha3 key) of
        Right a -> a
        Left e  -> error $ "Impossible error: " ++ e
  where
    sha3 :: PubKey -> ByteString
    sha3 = convert . hashWith Keccak_256 . BA.drop 1 . exportPubKey False

-- | Decode address from hex string
fromHexString :: HexString -> Either String Address
fromHexString bs
  | bslen == 20 = decode (zero 12 <> toBytes bs :: ByteString)
  | otherwise = Left $ "Incorrect address length: " ++ show bslen
  where bslen = C8.length (toBytes bs)

-- | Encode address to hex string
toHexString :: Address -> HexString
toHexString = fromBytes . C8.drop 12 . encode

-- | Encode address with mixed-case checksum
-- https://github.com/ethereum/EIPs/blob/master/EIPS/eip-55.md
toChecksum :: ByteString -> ByteString
toChecksum addr = ("0x" <>) . C8.pack $ zipWith ($) upcaseVector lower
  where
    upcaseVector = (>>= fourthBits) . BS.unpack . BS.take 20 . convert $ hashWith Keccak_256 (C8.pack lower)
    fourthBits n = bool id C.toUpper <$> [n .&. 0x80 /= 0, n .&. 0x08 /= 0]
    lower = drop 2 . fmap C.toLower . C8.unpack $ addr

-- | Verify mixed-case address checksum
-- https://github.com/ethereum/EIPs/blob/master/EIPS/eip-55.md
verifyChecksum :: ByteString -> Bool
verifyChecksum = toChecksum >>= (==)
