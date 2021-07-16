-- |
-- Module      :  Network.Polkadot.Query
-- Copyright   :  Aleksandr Krupenkin 2016-2021
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- Query storage for internal data.
--

module Network.Polkadot.Query where

import           Codec.Scale                  (Decode, decode)
import           Data.ByteArray.HexString     (HexString)
import           Data.Text                    (Text)
import           Network.JsonRpc.TinyClient   (JsonRpc)

import           Network.Polkadot.Metadata    (Metadata (Metadata),
                                               metadataTypes, toLatest)
import           Network.Polkadot.Rpc.State   (getMetadata, getStorage)
import           Network.Polkadot.Storage     (Storage, fromMetadata,
                                               storageKey)
import           Network.Polkadot.Storage.Key (Argument)

-- | Loads metadata from runtime and create storage type.
storage :: JsonRpc m => m (Either String Storage)
storage = (fmap go . decode) <$> getMetadata
  where
    go raw = let (meta, _) = metadataTypes raw in fromMetadata (toLatest meta)

-- | Query data from blockchain via 'getStorage' RPC call.
query :: (JsonRpc m, Decode a)
      => Text
      -- ^ Module name.
      -> Text
      -- ^ Storage method name.
      -> [Argument]
      -- ^ Arguments (for mappings).
      -> m (Either String a)
      -- ^ Decoded storage item.
{-# INLINE query #-}
query = query' Nothing

-- | Similar to 'query' but get block hash for query as an argument.
query' :: (JsonRpc m, Decode a)
       => Maybe HexString
       -- ^ Block hash for query ('Nothing' for best block).
       -> Text
       -- ^ Module name.
       -> Text
       -- ^ Storage method name.
       -> [Argument]
       -- ^ Arguments (for mappings).
       -> m (Either String a)
       -- ^ Decoded storage item.
query' blockHash section method args = go =<< storage
  where
    go (Right store) = case storageKey store section method args of
        Just key -> decode <$> getStorage key blockHash
        Nothing -> return (Left "Unable to find given section/method or wrong argument count")
    go (Left err) = return (Left err)
