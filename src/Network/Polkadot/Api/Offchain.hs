{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Polkadot.Api.Offchain
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Polkadot RPC methods with `offchain` prefix.
--

module Network.Polkadot.Api.Offchain where

import           Data.Text                  (Text)

import           Data.ByteArray.HexString   (HexString)
import           Network.JsonRpc.TinyClient (JsonRpc (..))
import           Network.Polkadot.Api.Types (StorageKind)

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
