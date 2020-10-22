{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Polkadot.Rpc.Offchain
-- Copyright   :  Aleksandr Krupenkin 2016-2020
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Polkadot RPC methods with `offchain` prefix.
--

module Network.Polkadot.Rpc.Offchain where

import           Data.ByteArray.HexString   (HexString)
import           Network.JsonRpc.TinyClient (JsonRpc (..))

import           Network.Polkadot.Rpc.Types (StorageKind)

-- | Get offchain local storage under given key and prefix.
localStorageGet :: JsonRpc m
                => StorageKind
                -- ^ Offchain storage kind
                -> HexString
                -- ^ Key
                -> m (Maybe HexString)
{-# INLINE localStorageGet #-}
localStorageGet = remote "offchain_localStorageGet"

-- | Set offchain local storage under given key and prefix
localStorageSet :: JsonRpc m
                => StorageKind
                -- ^ Offchain storage kind
                -> HexString
                -- ^ Key
                -> HexString
                -- ^ Value
                -> m ()
{-# INLINE localStorageSet #-}
localStorageSet = remote "offchain_localStorageSet"
