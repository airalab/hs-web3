-- |
-- Module      :  Network.Ethereum.Web3.Net
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unknown
--
-- Ethereum node JSON-RPC API methods with `net_` prefix.
--
module Network.Ethereum.Web3.Net where

import           Network.Ethereum.Web3.Monad
import           Network.Ethereum.Web3.Provider
import           Network.Ethereum.Web3.Types
import           Network.JsonRpc.TinyClient

-- | Returns the current network id.
version :: Web3 Int
{-# INLINE version #-}
version = remote "net_version"

-- | Returns true if client is actively listening for network connections.
listening :: Web3 Bool
{-# INLINE listening #-}
listening = remote "net_listening"

-- | Returns number of peers currently connected to the client.
peerCount :: Web3 Quantity
{-# INLINE peerCount #-}
peerCount = remote "net_peerCount"
