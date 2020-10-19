{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Polkadot.Api.State
-- Copyright   :  Aleksandr Krupenkin 2016-2020
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Polkadot RPC methods with `state` prefix.
--

module Network.Polkadot.Api.State where

import           Data.Text                  (Text)

import           Data.ByteArray.HexString   (HexString)
import           Network.JsonRpc.TinyClient (JsonRpc (..))
import           Network.Polkadot.Api.Types (ReadProof, RuntimeVersion,
                                             StorageChangeSet)

-- | Perform a call to a builtin on the chain.
call :: JsonRpc m
     => Text
     -- ^ Call method
     -> HexString
     -- ^ Call data
     -> Maybe HexString
     -- ^ Block hash or nothing for head
     -> m HexString
{-# INLINE call #-}
call = remote "state_call"

-- | Retrieves the keys with prefix of a specific child storage.
getChildKeys :: JsonRpc m
             => HexString
             -- ^ Child storage key
             -> HexString
             -- ^ Child definition
             -> Int
             -- ^ Child type
             -> HexString
             -- ^ Key
             -> Maybe HexString
             -- ^ Block hash or nothing for head
             -> m [HexString]
{-# INLINE getChildKeys #-}
getChildKeys = remote "state_getChildKeys"

-- | Retrieves the child storage for a key.
getChildStorage :: JsonRpc m
                => HexString
                -- ^ Child storage key
                -> HexString
                -- ^ Child definition
                -> Int
                -- ^ Child type
                -> HexString
                -- ^ Key
                -> Maybe HexString
                -- ^ Block hash or nothing for head
                -> m HexString
{-# INLINE getChildStorage #-}
getChildStorage = remote "state_getChildStorage"

-- | Retrieves the child storage hash.
getChildStorageHash :: JsonRpc m
                    => HexString
                    -- ^ Child storage key
                    -> HexString
                    -- ^ Child definition
                    -> Int
                    -- ^ Child type
                    -> HexString
                    -- ^ Key
                    -> Maybe HexString
                    -- ^ Block hash or nothing for head
                    -> m HexString
{-# INLINE getChildStorageHash #-}
getChildStorageHash = remote "state_getChildStorageHash"

-- | Retrieves the child storage size.
getChildStorageSize :: JsonRpc m
                    => HexString
                    -- ^ Child storage key
                    -> HexString
                    -- ^ Child definition
                    -> Int
                    -- ^ Child type
                    -> HexString
                    -- ^ Key
                    -> Maybe HexString
                    -- ^ Block hash or nothing for head
                    -> m Int
{-# INLINE getChildStorageSize #-}
getChildStorageSize = remote "state_getChildStorageSize"

-- | Retrieves the keys with a certain prefix.
getKeys :: JsonRpc m
        => HexString
        -- ^ Key
        -> Maybe HexString
        -- ^ Block hash or nothing for head
        -> m [HexString]
{-# INLINE getKeys #-}
getKeys = remote "state_getKeys"

-- | Returns the runtime metadata.
getMetadata :: JsonRpc m => m HexString
{-# INLINE getMetadata #-}
getMetadata = remote "state_getMetadata"

-- | Returns proof of storage entries at a specific block state.
getReadProof :: JsonRpc m
             => [HexString]
             -- ^ Keys
             -> Maybe HexString
             -- ^ Block hash or nothing for head
             -> m ReadProof
{-# INLINE getReadProof #-}
getReadProof = remote "state_getReadProof"

-- | Get runtime version.
getRuntimeVersion :: JsonRpc m
                  => Maybe HexString
                  -- ^ Block hash or nothing for head
                  -> m RuntimeVersion
{-# INLINE getRuntimeVersion #-}
getRuntimeVersion = remote "state_getRuntimeVersion"

-- | Retrieves the storage for a key.
getStorage :: JsonRpc m
           => HexString
           -- ^ Key
           -> Maybe HexString
           -- ^ Block hash or nothing for head
           -> m HexString
{-# INLINE getStorage #-}
getStorage = remote "state_getStorage"

-- | Retrieves the storage hash.
getStorageHash :: JsonRpc m
               => HexString
               -- ^ Key
               -> Maybe HexString
               -- ^ Block hash or nothing for head
               -> m HexString
{-# INLINE getStorageHash #-}
getStorageHash = remote "state_getStorageHash"

-- | Retrieves the storage size.
getStorageSize :: JsonRpc m
               => HexString
               -- ^ Key
               -> Maybe HexString
               -- ^ Block hash or nothing for head
               -> m Int
{-# INLINE getStorageSize #-}
getStorageSize = remote "state_getStorageSize"

-- | Query historical storage entries (by key) starting from a start block.
queryStorage :: JsonRpc m
             => [HexString]
             -- ^ Storage keys
             -> HexString
             -- ^ From block hash
             -> Maybe HexString
             -- ^ To block hash
             -> m [StorageChangeSet]
{-# INLINE queryStorage #-}
queryStorage = remote "state_queryStorage"

-- | Query storage entries (by key) starting at block hash given as the second parameter.
queryStorageAt :: JsonRpc m
               => [HexString]
               -- ^ Storage keys
               -> Maybe HexString
               -- ^ Block hash or nothing for head
               -> m [StorageChangeSet]
{-# INLINE queryStorageAt #-}
queryStorageAt = remote "state_queryStorageAt"
