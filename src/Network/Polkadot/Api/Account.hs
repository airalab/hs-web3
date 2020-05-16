{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Polkadot.Api.Account
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Polkadot RPC methods with `account` prefix.
--

module Network.Polkadot.Api.Account where

import           Data.Text                  (Text)

import           Network.JsonRpc.TinyClient (JsonRpc (..))

-- | Retrieves the next accountIndex as available on the node.
nextIndex :: JsonRpc m
          => Text
          -- ^ AccountId
          -> m Int
{-# INLINE nextIndex #-}
nextIndex = remote "account_nextIndex"
