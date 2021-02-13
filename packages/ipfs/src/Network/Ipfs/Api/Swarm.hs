-- |
-- Module      :  Network.Ipfs.Api.Swarm
-- Copyright   :  Aleksandr Krupenkin 2016-2021
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unknown
--
-- Api calls with `swarm` prefix.
--

module Network.Ipfs.Api.Swarm where

import           Control.Monad.IO.Class         (MonadIO)
import           Data.Text                      (Text)

import           Network.Ipfs.Api.Internal      (_swarmConnect,
                                                 _swarmDisconnect,
                                                 _swarmFilterAdd,
                                                 _swarmFilterRm, _swarmFilters,
                                                 _swarmPeers)
import           Network.Ipfs.Api.Internal.Call (call)
import           Network.Ipfs.Api.Types         (SwarmObj, SwarmPeersObj)
import           Network.Ipfs.Client            (IpfsT)

-- | List peers with open connections.
swarmPeers :: MonadIO m => IpfsT m SwarmPeersObj
swarmPeers = call _swarmPeers

-- | Open connection to a given address. 'peerId' has to be of the format - /ipfs/id
connect :: MonadIO m => Text -> IpfsT m SwarmObj
connect = call . _swarmConnect . Just

-- | Close connection to a given address. 'peerId' has to be of the format - /ipfs/id
disconnect :: MonadIO m => Text -> IpfsT m SwarmObj
disconnect = call . _swarmDisconnect . Just

-- | Manipulate address filters.
filters :: MonadIO m => IpfsT m SwarmObj
filters = call _swarmFilters

-- | Add an address filter. 'peerId' has to be of the format - /ip4/{IP addr of peer}/ipcidr/{ip network prefix}
filterAdd :: MonadIO m => Text -> IpfsT m SwarmObj
filterAdd = call . _swarmFilterAdd . Just

-- | Remove an address filter.
filterRm :: MonadIO m => Text -> IpfsT m SwarmObj
filterRm = call . _swarmFilterRm . Just
