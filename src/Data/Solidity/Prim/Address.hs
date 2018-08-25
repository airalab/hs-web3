{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Data.Solidity.Prim.Address
-- Copyright   :  Alexander Krupenkin 2016-2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  noportable
--
-- Ethreum account address.
--

module Data.Solidity.Prim.Address (
    Address
  , toHexString
  , fromHexString
  , toChecksum
  , verifyChecksum
  ) where

import           Control.Monad           ((<=<))
import           Crypto.Hash             (Keccak_256 (..), hashWith)
import           Data.Aeson              (FromJSON (..), ToJSON (..))
import           Data.Bits               ((.&.))
import           Data.Bool               (bool)
import           Data.ByteArray          (convert, zero)
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as BS (take, unpack)
import qualified Data.ByteString.Char8   as C8 (drop, length, pack, unpack)
import qualified Data.Char               as C (toLower, toUpper)
import           Data.HexString          (HexString, fromBytes, hexString,
                                          toBytes)
import           Data.String             (IsString (..))
import           Generics.SOP            (Generic)
import qualified GHC.Generics            as GHC (Generic)

import           Data.Solidity.Abi       (AbiGet (..), AbiPut (..),
                                          AbiType (..))
import           Data.Solidity.Abi.Codec (decode, encode)
import           Data.Solidity.Prim.Int  (UIntN)

-- | Ethereum account address
newtype Address = Address { unAddress :: UIntN 160 }
  deriving (Eq, Ord, GHC.Generic)

instance Generic Address

fromHexString :: HexString -> Either String Address
fromHexString bs
  | bslen == 20 = decode (zero 12 <> toBytes bs :: ByteString)
  | otherwise = Left $ "Incorrect address length: " ++ show bslen
  where bslen = C8.length (toBytes bs)

toHexString :: Address -> HexString
toHexString = fromBytes . C8.drop 12 . encode

toChecksum :: ByteString -> ByteString
toChecksum addr = ("0x" <>) . C8.pack $ zipWith ($) upcaseVector lower
  where
    upcaseVector = (>>= fourthBits) . BS.unpack . BS.take 20 . convert $ hashWith Keccak_256 (C8.pack lower)
    fourthBits n = bool id C.toUpper <$> [n .&. 0x80 /= 0, n .&. 0x08 /= 0]
    lower = drop 2 . fmap C.toLower . C8.unpack $ addr

verifyChecksum :: ByteString -> Bool
verifyChecksum = toChecksum >>= (==)

instance Show Address where
    show = show . toChecksum . toBytes . toHexString

instance IsString Address where
    fromString = either error id . (fromHexString <=< hexString) . fromString

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
