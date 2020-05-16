{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Polkadot.Api.Engine
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Polkadot RPC methods with `engine` prefix.
--

module Network.Polkadot.Api.Engine where

import           Data.Text                  (Text)

import           Network.JsonRpc.TinyClient (JsonRpc (..))
import           Network.Polkadot.Api.Types (CreatedBlock)

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
