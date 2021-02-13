{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Polkadot.Rpc.Engine
-- Copyright   :  Aleksandr Krupenkin 2016-2021
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Polkadot RPC methods with `engine` prefix.
--

module Network.Polkadot.Rpc.Engine where

import           Data.ByteArray.HexString   (HexString)
import           Network.JsonRpc.TinyClient (JsonRpc (..))

import           Network.Polkadot.Rpc.Types (CreatedBlock)

-- | Instructs the manual-seal authorship task to create a new block.
createBlock :: JsonRpc m
            => Bool
            -- ^ Create empty
            -> Bool
            -- ^ Finalize
            -> Maybe HexString
            -- ^ Parent hash
            -> m CreatedBlock
{-# INLINE createBlock #-}
createBlock = remote "engine_createBlock"

-- | Instructs the manual-seal authorship task to finalize a block.
finalizeBlock :: JsonRpc m
              => HexString
              -- ^ Block hash
              -> Maybe HexString
              -- ^ Justification
              -> m Bool
{-# INLINE finalizeBlock #-}
finalizeBlock = remote "engine_finalizeBlock"
