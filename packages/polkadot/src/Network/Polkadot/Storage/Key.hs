{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      :  Network.Polkadot.Storage.Key
-- Copyright   :  Aleksandr Krupenkin 2016-2021
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- When you use the Substrate RPC to access a storage item,
-- you only need to provide the key associated with that item.
--

module Network.Polkadot.Storage.Key where

import           Codec.Scale                   (encode)
import           Codec.Scale.Class             (Encode (..))
import           Control.Arrow                 ((&&&))
import           Data.ByteString               (ByteString)
import           Data.Digest.Blake2            (blake2_128, blake2_256)
import           Data.Digest.XXHash            (xxhash)
import           Data.Text                     (Text)
import           Data.Text.Encoding            (encodeUtf8)

import           Network.Polkadot.Metadata.V11 (DoubleMapType (..),
                                                MapType (..),
                                                StorageEntryMetadata (..),
                                                StorageEntryType (..),
                                                StorageHasher (..))

-- | General type wrapper for SCALE encodable storage index argument.
data Argument where
    Argument :: Encode a => a -> Argument
    -- ^ Wrapped type should be encodable.

instance Encode Argument where
    put arg = case arg of Argument a -> put a

-- | Hasher is a function that hash given argument.
type Hasher = Argument -> ByteString

-- | Entry type describe storage prefix for different storage entity types.
data StorageEntry
    = PlainEntry ByteString
    -- ^ Simple storage type without arguments.
    | MapEntry (Argument -> ByteString)
    -- ^ Mapping with hashing for arguments.
    | DoubleMapEntry (Argument -> Argument -> ByteString)
    -- ^ Double map with two different hashers.

instance Show StorageEntry where
    show (PlainEntry _)     = "PlainEntry"
    show (MapEntry _)       = "MapEntry"
    show (DoubleMapEntry _) = "DoubleMapEntry"

-- | Create storage key generator from metadata description.
newEntry :: Text
         -- ^ Storage prefix (module name).
         -> StorageEntryMetadata
         -- ^ Storage key metadata, includes entry type, name, etc.
         -> StorageEntry
         -- ^ Storage key generator.
newEntry prefix meta = case entryType meta of
    Plain _ -> PlainEntry plainKey
    Map MapType{..} -> MapEntry (mapCodec mapHasher)
    DoubleMap DoubleMapType{..} -> DoubleMapEntry (dMapCodec doubleMapHasher doubleMapKey2Hasher)
  where
    method = entryName meta
    -- To calculate the key for a simple Storage Value,
    -- take the TwoX 128 hash of the name of the module that contains the Storage Value
    -- and append to it the TwoX 128 hash of the name of the Storage Value itself.
    plainKey = xxhash 128 (encodeUtf8 prefix) <> xxhash 128 (encodeUtf8 method)
    -- Like Storage Values, the keys for Storage Maps are equal to the TwoX 128 hash
    -- of the name of the module that contains the map prepended to the TwoX 128 hash
    -- of the name of the Storage Map itself.
    mapCodec h1 arg1 = plainKey <> getHasher h1 arg1
    dMapCodec h1 h2 arg1 arg2 = mapCodec h1 arg1 <> getHasher h2 arg2

getHasher :: StorageHasher -> Hasher
getHasher Blake2_128       = blake2_128 . encode
getHasher Blake2_256       = blake2_256 . encode
getHasher Blake2_128Concat = uncurry (<>) . (blake2_128 &&& id) . encode
getHasher Twox128          = xxhash 128 . encode
getHasher Twox256          = xxhash 256 . encode
getHasher Twox64Concat     = uncurry (<>) . (xxhash 64 &&& id) . encode
getHasher Identity         = encode
