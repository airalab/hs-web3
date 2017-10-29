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

import Network.Ethereum.Web3.Provider
import Network.Ethereum.Web3.JsonRpc
import Network.Ethereum.Web3.Types
import Data.Text (Text)

-- | Returns the current network id.
version :: Provider a => Web3 a Text
{-# INLINE version #-}
version = remote "net_version"

-- | Returns true if client is actively listening for network connections.
listening :: Provider a => Web3 a Bool
{-# INLINE listening #-}
listening = remote "net_listening"

-- | Returns number of peers currently connected to the client.
peerCount :: Provider a => Web3 a Text
{-# INLINE peerCount #-}
peerCount = remote "net_peerCount"
