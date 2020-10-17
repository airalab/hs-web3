-- |
-- Module      :  Network.Ipfs.Api.Pubsub
-- Copyright   :  Aleksandr Krupenkin 2016-2020
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unknown
--
-- Api calls with `pubsub` prefix.
--

module Network.Ipfs.Api.Pubsub where

import           Control.Monad.IO.Class         (MonadIO)
import           Data.Text                      (Text)
import           Servant.API.ContentTypes       (NoContent)

import           Network.Ipfs.Api.Internal      (_pubsubLs, _pubsubPeers,
                                                 _pubsubPublish)
import           Network.Ipfs.Api.Internal.Call (call)
import           Network.Ipfs.Api.Types         (PubsubObj)
import           Network.Ipfs.Client            (IpfsT)

-- | List subscribed topics by name.
ls :: MonadIO m => IpfsT m PubsubObj
ls = call _pubsubLs

-- | List peers we are currently pubsubbing with.
peers :: MonadIO m => IpfsT m PubsubObj
peers = call _pubsubPeers

-- | Publish a message to a given pubsub topic.
publish :: MonadIO m => Text -> Text -> IpfsT m NoContent
publish topic = call . _pubsubPublish topic . Just

-- | Subscribe to messages on a given topic.
--subscribe :: Text -> m ()
--subscribe = pubsubCall . _pubsubSubscribe
