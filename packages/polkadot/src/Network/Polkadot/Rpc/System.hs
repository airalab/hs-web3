{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Polkadot.Rpc.System
-- Copyright   :  Aleksandr Krupenkin 2016-2024
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Polkadot RPC methods with `system` prefix.
--

module Network.Polkadot.Rpc.System where

import           Data.Aeson                 (Object)
import           Data.Text                  (Text)
import           Network.JsonRpc.TinyClient (JsonRpc (..))

import           Network.Polkadot.Rpc.Types (ChainType, Health, NodeRole,
                                             PeerInfo)

-- | Adds a reserved peer.
addReservedPeer :: JsonRpc m
                => Text
                -- ^ Peer URI
                -> m Text
{-# INLINE addReservedPeer #-}
addReservedPeer = remote "system_addReservedPeer"

-- | Retrieves the chain.
chain :: JsonRpc m => m Text
{-# INLINE chain #-}
chain = remote "system_chain"

-- | Retrieves the chain type.
chainType :: JsonRpc m => m ChainType
{-# INLINE chainType #-}
chainType = remote "system_chainType"

-- | Return health status of the node.
health :: JsonRpc m => m Health
{-# INLINE health #-}
health = remote "system_health"

-- | The addresses include a trailing /p2p/ with the local PeerId,
-- and are thus suitable to be passed to addReservedPeer or as a bootnode address.
localListenAddresses :: JsonRpc m => m [Text]
{-# INLINE localListenAddresses #-}
localListenAddresses = remote "system_localListenAddresses"

-- | Returns the base58-encoded PeerId of the node.
localPeerId :: JsonRpc m => m Text
{-# INLINE localPeerId #-}
localPeerId = remote "system_localPeerId"

-- | Retrieves the node name.
name :: JsonRpc m => m Text
{-# INLINE name #-}
name = remote "system_name"

-- | Returns current state of the network.
--
-- Warning: This API isn't stable.
networkState :: JsonRpc m => m Object
{-# INLINE networkState #-}
networkState = remote "system_networkState"

-- | Returns the roles the node is running as.
nodeRoles :: JsonRpc m => m [NodeRole]
{-# INLINE nodeRoles #-}
nodeRoles = remote "system_nodeRoles"

-- | Returns the currently connected peers.
peers :: JsonRpc m => m [PeerInfo]
{-# INLINE peers #-}
peers = remote "system_peers"

-- | Get a custom set of properties as a JSON object, defined in the chain spec.
properties :: JsonRpc m => m Object
{-# INLINE properties #-}
properties = remote "system_properties"

-- | Remove a reserved peer.
removeReservedPeer :: JsonRpc m
                   => Text
                   -- ^ Peer URI
                   -> m Text
{-# INLINE removeReservedPeer #-}
removeReservedPeer = remote "system_removeReservedPeer"

-- | Retrieves the version of the node.
version :: JsonRpc m => m Text
{-# INLINE version #-}
version = remote "system_version"
