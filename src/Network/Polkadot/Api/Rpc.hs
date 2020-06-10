{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Polkadot.Api.Rpc
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Polkadot RPC methods with `rpc` prefix.
--

module Network.Polkadot.Api.Rpc where

import           Data.Aeson                 (Value)

import           Network.JsonRpc.TinyClient (JsonRpc (..))

-- | Retrieves the list of RPC methods that are exposed by the node.
methods :: JsonRpc m => m Value
{-# INLINE methods #-}
methods = remote "rpc_methods"
