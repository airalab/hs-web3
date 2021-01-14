{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      :  Network.Polkadot.Storage.Key
-- Copyright   :  Aleksandr Krupenkin 2016-2020
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- A representation of a storage key (typically hashed) in the system. It can be
-- constructed by passing in a raw key or a StorageEntry with (optional) arguments.
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

-- | Hasher is a function that hash given argument.
type Hasher a = a -> ByteString

-- | General type wrapper for SCALE encodable data type.
data Argument where
    Argument :: Encode a => a -> Argument
    -- ^ Wrapped type should be encodable.

instance Encode Argument where
    put arg = case arg of Argument a -> put a

-- | Entry type describe storage prefix for different storage entity types.
data StorageEntry where
    PlainEntry :: ByteString -> StorageEntry
    -- ^ Simple storage type without arguments.
    MapEntry :: (Argument -> ByteString) -> StorageEntry
    -- ^ Mapping with hashing for arguments.
    DoubleMapEntry :: (Argument -> Argument -> ByteString) -> StorageEntry
    -- ^ Double map with two different hashers.

instance Show StorageEntry where
    show (PlainEntry _)     = "PlainEntry"
    show (MapEntry _)       = "MapEntry"
    show (DoubleMapEntry _) = "DoubleMapEntry"

newEntry :: Text -> StorageEntryMetadata -> Text -> StorageEntry
newEntry prefix meta method = case entryType meta of
    Plain _ -> PlainEntry plainKey
    Map MapType{..} -> MapEntry $ mapCodec (getHasher mapHasher)
    DoubleMap DoubleMapType{..} ->
        DoubleMapEntry $ dMapCodec (getHasher doubleMapHasher) (getHasher doubleMapKey2Hasher)
  where
    plainKey = xxhash 128 (encodeUtf8 prefix) <> xxhash 128 (encodeUtf8 method)
    mapCodec hasher arg = plainKey <> hasher arg
    dMapCodec hasher1 hasher2 arg1 arg2 = plainKey <> hasher1 arg1 <> hasher2 arg2

getHasher :: Encode a => StorageHasher -> Hasher a
getHasher Blake2_128       = blake2_128 . encode
getHasher Blake2_256       = blake2_256 . encode
getHasher Blake2_128Concat = uncurry (<>) . (blake2_128 &&& id) . encode
getHasher Twox128          = xxhash 128 . encode
getHasher Twox256          = xxhash 256 . encode
getHasher Twox64Concat     = uncurry (<>) . (xxhash 64 &&& id) . encode
getHasher Identity         = encode
