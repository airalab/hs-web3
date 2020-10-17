-- |
-- Module      :  Network.Ipfs.Api.Internal.Stream
-- Copyright   :  Aleksandr Krupenkin 2016-2020
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Ipfs Stream API provider.
--

module Network.Ipfs.Api.Internal.Stream where

import qualified Data.ByteString.Lazy.Char8    ()
import           Data.Proxy
import           Data.Text
import           Network.HTTP.Client           ()
import           Servant.API
import           Servant.Client.Streaming      as S

import           Network.Ipfs.Api.Types.Stream

_ipfsStreamApi :: Proxy IpfsStreamApi
_ipfsStreamApi =  Proxy

_ping :: Text -> ClientM (SourceIO PingObj)
_dhtFindPeer :: Text -> ClientM (SourceIO DhtObj)
_dhtFindProvs :: Text -> ClientM (SourceIO DhtObj)
_dhtGet :: Text -> ClientM (SourceIO DhtObj)
_dhtProvide :: Text -> ClientM (SourceIO DhtObj)
_dhtQuery :: Text -> ClientM (SourceIO DhtObj)
_logTail :: ClientM (SourceIO LogReturnType)
_repoGc :: ClientM (SourceIO RepoGcObj)
_repoVerify :: ClientM (SourceIO RepoVerifyObj)
_refs :: Text -> ClientM (SourceIO RefsObj)
_refsLocal :: ClientM (SourceIO RefsObj)
_pubsubSubscribe :: Text -> ClientM (SourceIO PubsubSubObj)

_ping :<|> _dhtFindPeer :<|> _dhtFindProvs :<|> _dhtGet :<|> _dhtProvide :<|> _dhtQuery :<|>
  _logTail :<|> _repoGc :<|> _repoVerify :<|> _refs :<|> _refsLocal :<|> _pubsubSubscribe = client _ipfsStreamApi
