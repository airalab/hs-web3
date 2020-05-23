-- |
-- Module      :  Network.Ipfs.Api.Dht
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unknown
--
-- Api calls with `dht` prefix.
--

module Network.Ipfs.Api.Dht where

import           Control.Monad.IO.Class           (MonadIO)
import           Data.Text                        (Text)

import           Network.Ipfs.Api.Internal.Call   (streamCall)
import           Network.Ipfs.Api.Internal.Stream (_dhtFindPeer, _dhtFindProvs,
                                                   _dhtGet, _dhtProvide,
                                                   _dhtQuery)

-- | Find the multiaddresses associated with the given peerId.
findPeer :: MonadIO m => Text -> m ()
findPeer = streamCall . _dhtFindPeer

-- | Find peers that can provide a specific value, given a key.
findProvs :: MonadIO m => Text -> m ()
findProvs = streamCall . _dhtFindProvs

-- | Given a key, query the routing system for its best value.
get :: MonadIO m => Text -> m ()
get cid = streamCall $ _dhtGet cid

-- | Announce to the network that you are providing given values.
provide :: MonadIO m => Text -> m ()
provide = streamCall . _dhtProvide

-- | Find the closest Peer IDs to a given peerID by querying the DHT.
query :: MonadIO m => Text -> m ()
query = streamCall . _dhtQuery
