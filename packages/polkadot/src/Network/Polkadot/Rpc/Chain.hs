{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Polkadot.Rpc.Chain
-- Copyright   :  Aleksandr Krupenkin 2016-2024
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Polkadot RPC methods with `chain` prefix.
--

module Network.Polkadot.Rpc.Chain where

import           Data.ByteArray.HexString   (HexString)
import           Network.JsonRpc.TinyClient (JsonRpc (..))

import           Network.Polkadot.Rpc.Types (Header, SignedBlock)

-- | Get header and body of a relay chain block.
getBlock :: JsonRpc m
         => Maybe HexString
         -- ^ Block hash
         -> m (Maybe SignedBlock)
{-# INLINE getBlock #-}
getBlock = remote "chain_getBlock"

-- | Get the block hash for a specific block.
getBlockHash :: JsonRpc m
             => Maybe Int
             -- ^ Block number
             -> m (Maybe HexString)
{-# INLINE getBlockHash #-}
getBlockHash = remote "chain_getBlockHash"

-- | Get hash of the last finalized block in the canon chain.
getFinalizedHead :: JsonRpc m => m HexString
{-# INLINE getFinalizedHead #-}
getFinalizedHead = remote "chain_getFinalizedHead"

-- | Retrieves the header for a specific block.
getHeader :: JsonRpc m
          => Maybe HexString
          -- ^ Block hash
          -> m (Maybe Header)
{-# INLINE getHeader #-}
getHeader = remote "chain_getHeader"
