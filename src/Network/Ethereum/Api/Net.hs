{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Ethereum.Api.Net
-- Copyright   :  Alexander Krupenkin 2016-2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unknown
--
-- Ethereum node JSON-RPC API methods with `net_` prefix.
--

module Network.Ethereum.Api.Net where

import           Data.Text                  (Text)
import           Network.Ethereum.Api.Types (Quantity)
import           Network.JsonRpc.TinyClient (JsonRpcM, remote)

-- | Returns the current network id.
version :: JsonRpcM m => m Text
{-# INLINE version #-}
version = remote "net_version"

-- | Returns true if client is actively listening for network connections.
listening :: JsonRpcM m => m Bool
{-# INLINE listening #-}
listening = remote "net_listening"

-- | Returns number of peers currently connected to the client.
peerCount :: JsonRpcM m => m Quantity
{-# INLINE peerCount #-}
peerCount = remote "net_peerCount"
