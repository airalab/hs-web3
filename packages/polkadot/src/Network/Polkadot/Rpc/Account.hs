{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Polkadot.Rpc.Account
-- Copyright   :  Aleksandr Krupenkin 2016-2021
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Polkadot RPC methods with `account` prefix.
--

module Network.Polkadot.Rpc.Account where

import           Data.Text                  (Text)
import           Network.JsonRpc.TinyClient (JsonRpc (..))

-- | Retrieves the next accountIndex as available on the node.
nextIndex :: JsonRpc m
          => Text
          -- ^ AccountId
          -> m Int
{-# INLINE nextIndex #-}
nextIndex = remote "account_nextIndex"
