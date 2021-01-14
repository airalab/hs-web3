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
-- .
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

type Storage = Map Text ModuleStorage
type ModuleStorage = Map Text StorageEntry

fromMetadata :: Metadata -> Storage
fromMetadata = Map.fromList . mapMaybe go . modules
  where
    toLowerFirst = uncurry T.cons . (toLower . T.head &&& T.tail)
    go ModuleMetadata{..} = do
        StorageMetadata prefix items <- moduleStorage
        let section = toCamelCase moduleName
            toEntry meta@StorageEntryMetadata{..} =
                (toLowerFirst entryName, newEntry prefix meta entryName)
        return (toLowerFirst section, Map.fromList $ fmap toEntry items)

getPrefix :: Storage -> Text -> Text -> [Argument] -> Maybe HexString
getPrefix store section method args = convert <$> do
    entries <- Map.lookup section store
    entry <- Map.lookup method entries
    case entry of
      PlainEntry x     -> Just x
      MapEntry f       -> case args of
                            [a] -> Just (f a)
                            _   -> Nothing
      DoubleMapEntry f -> case args of
                            [a, b] -> Just (f a b)
                            _      -> Nothing
