{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Data.Solidity.Prim.Address
-- Copyright   :  Aleksandr Krupenkin 2016-2021
-- License     :  Apache-2.0
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

    -- * Derive address from public key
    , fromPubKey

    -- * EIP55 Mix-case checksum address encoding
    , toChecksum
    , verifyChecksum
    ) where

import           Control.Monad            ((<=<))
import           Crypto.Ethereum          (PublicKey)
import           Data.Aeson               (FromJSON (..), ToJSON (..))
import           Data.Bits                ((.&.))
import           Data.Bool                (bool)
import           Data.ByteArray           (zero)
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

import           Crypto.Ecdsa.Utils       (exportPubKey)
import           Crypto.Ethereum.Utils    (keccak256)
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
fromPubKey :: PublicKey -> Address
fromPubKey key =
    case decode $ zero 12 <> toAddress (exportPubKey key) of
        Right a -> a
        Left e  -> error $ "Impossible error: " ++ e
  where
    toAddress :: HexString -> HexString
    toAddress = BA.drop 12 . keccak256

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
    upcaseVector = (>>= fourthBits) . BS.unpack . BS.take 20 $ keccak256 (C8.pack lower)
    fourthBits n = bool id C.toUpper <$> [n .&. 0x80 /= 0, n .&. 0x08 /= 0]
    lower = drop 2 . fmap C.toLower . C8.unpack $ addr

-- | Verify mixed-case address checksum
-- https://github.com/ethereum/EIPs/blob/master/EIPS/eip-55.md
verifyChecksum :: ByteString -> Bool
verifyChecksum = toChecksum >>= (==)
