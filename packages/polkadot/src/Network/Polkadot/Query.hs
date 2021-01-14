-- |
-- Module      :  Network.Polkadot.Query
-- Copyright   :  Aleksandr Krupenkin 2016-2020
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
import           Data.Text                    (Text)
import           Network.JsonRpc.TinyClient   (JsonRpc)

import           Network.Polkadot.Metadata    (Metadata (Metadata),
                                               MetadataVersioned (V12),
                                               metadataTypes)
import           Network.Polkadot.Rpc.State   (getMetadata, getStorage)
import           Network.Polkadot.Storage     (Storage, fromMetadata, getPrefix)
import           Network.Polkadot.Storage.Key (Argument)

storage :: JsonRpc m => m (Either String Storage)
storage = (fmap go . decode) <$> getMetadata
  where
    go raw = let (Metadata _ (V12 meta), _) = metadataTypes raw in fromMetadata meta

query :: (JsonRpc m, Decode a) => Text -> Text -> [Argument] -> m (Either String a)
query section method args = do
    mbstore <- storage
    case mbstore of
      Right store -> case getPrefix store section method args of
                       Nothing -> return (Left "Unable to find given section/method or wrong argument count")
                       Just prefix -> decode <$> getStorage prefix Nothing
      Left err    -> return (Left err)
