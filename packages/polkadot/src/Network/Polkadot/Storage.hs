{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      :  Network.Polkadot.Storage
-- Copyright   :  Aleksandr Krupenkin 2016-2020
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Substrate uses a simple key-value data store implemented as a database-backed,
-- modified Merkle tree.
--
-- Blockchains that are built with Substrate expose a remote procedure call (RPC)
-- server that can be used to query runtime storage.
--

module Network.Polkadot.Storage where

import           Control.Arrow                 ((&&&))
import           Data.ByteArray                (convert)
import           Data.ByteArray.HexString      (HexString)
import           Data.Char                     (toLower)
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as Map (fromList, lookup)
import           Data.Maybe                    (mapMaybe)
import           Data.Text                     (Text)
import qualified Data.Text                     as T (cons, head, tail)
import           Text.AnimalCase               (toCamelCase)

import           Network.Polkadot.Metadata.V11 (StorageEntryMetadata (..),
                                                StorageMetadata (..))
import           Network.Polkadot.Metadata.V12 (Metadata (modules),
                                                ModuleMetadata (..))
import           Network.Polkadot.Storage.Key  (Argument, StorageEntry (..),
                                                newEntry)

-- | Runtime storage is a set of named modules.
type Storage = Map Text ModuleStorage

-- | Each module store data in a set of named entries.
type ModuleStorage = Map Text StorageEntry

-- | Create 'Storage' abstraction from runtime metadata.
fromMetadata :: Metadata
             -- ^ Runtime metadata (latest version).
             -> Storage
             -- ^ Storage entities.
fromMetadata = Map.fromList . mapMaybe go . modules
  where
    toLowerFirst = uncurry T.cons . (toLower . T.head &&& T.tail)
    go ModuleMetadata{..} = do
        StorageMetadata prefix items <- moduleStorage
        let section = toCamelCase moduleName
            toEntry meta@StorageEntryMetadata{..} =
                (toLowerFirst entryName, newEntry prefix meta)
        return (toLowerFirst section, Map.fromList $ fmap toEntry items)

-- | Create storage key for given parameters.
storageKey :: Storage
           -- ^ Storage entities.
           -> Text
           -- ^ Module name.
           -> Text
           -- ^ Storage method name.
           -> [Argument]
           -- ^ Arguments (for mappings).
           -> Maybe HexString
           -- ^ Raw storage key. If module or method was not found
           -- or wrong number of arguments - returns 'Nothing'.
storageKey store section method args = convert <$> do
    entry <- Map.lookup method =<< Map.lookup section store
    case entry of
      PlainEntry x     -> Just x
      MapEntry f       -> case args of
                            [a] -> Just (f a)
                            _   -> Nothing
      DoubleMapEntry f -> case args of
                            [a, b] -> Just (f a b)
                            _      -> Nothing
