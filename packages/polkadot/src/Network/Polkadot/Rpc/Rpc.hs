{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Polkadot.Rpc.Rpc
-- Copyright   :  Aleksandr Krupenkin 2016-2021
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Polkadot RPC methods with `rpc` prefix.
--

module Network.Polkadot.Rpc.Rpc where

import           Data.Aeson                 (Value)
import           Network.JsonRpc.TinyClient (JsonRpc (..))

-- | Retrieves the list of RPC methods that are exposed by the node.
methods :: JsonRpc m => m Value
{-# INLINE methods #-}
methods = remote "rpc_methods"
