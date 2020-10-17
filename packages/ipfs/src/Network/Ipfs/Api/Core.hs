{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Ipfs.Api.Core
-- Copyright   :  Aleksandr Krupenkin 2016-2020
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unknown
--
-- Core IPFS API calls.
--

module Network.Ipfs.Api.Core where

import qualified Codec.Archive.Tar                as Tar
import           Control.Monad.IO.Class           (MonadIO (..))
import           Data.Aeson                       (decode)
import           Data.ByteString.Lazy             (fromStrict)
import           Data.Text                        (Text)
import           Data.Text.Encoding               (encodeUtf8)
import           Network.HTTP.Client              (responseBody)
import           Servant.API.ContentTypes         (NoContent)

import           Network.Ipfs.Api.Internal        (_cat, _dns, _get, _id,
                                                   _idPeer, _ls, _shutdown,
                                                   _version)
import           Network.Ipfs.Api.Internal.Call   (call, multipartCall,
                                                   streamCall)
import           Network.Ipfs.Api.Internal.Stream (_ping, _refs, _refsLocal)
import           Network.Ipfs.Api.Types           (AddObj, CatReturnType,
                                                   DnsObj, IdObj, LsObj,
                                                   VersionObj)
import           Network.Ipfs.Client              (IpfsT)

-- | Show IPFS object data.
cat :: MonadIO m => Text -> IpfsT m CatReturnType
cat = call . _cat

-- | Add a file or directory to ipfs.
add :: MonadIO m => Text -> IpfsT m (Maybe AddObj)
add = fmap decodeResponse . multipartCall "add"
  where
    decodeResponse = decode . responseBody

-- | List directory contents for Unix filesystem objects.
ls :: MonadIO m => Text -> IpfsT m LsObj
ls = call . _ls

-- | Download IPFS objects.
get :: MonadIO m => Text -> IpfsT m Text
get hash = do
    ret <- call $ _get hash
    do liftIO $ Tar.unpack "getResponseDirectory" . Tar.read . fromStrict $ encodeUtf8 ret
       pure "The content has been stored in getResponseDirectory."

-- | Show ipfs version information.
version :: MonadIO m => IpfsT m VersionObj
version = call _version

-- | Show ipfs node id info.
id :: MonadIO m => IpfsT m IdObj
id = call _id

-- | Show ipfs node id info of the given peerId.
idPeer :: MonadIO m => Text -> IpfsT m IdObj
idPeer = call . _idPeer

-- | Resolve DNS links.
dns :: MonadIO m => Text -> IpfsT m DnsObj
dns = call . _dns

-- | List links (references) from an object.
refs :: MonadIO m => Text -> m ()
refs = streamCall . _refs

-- | List all local references.
refsLocal :: MonadIO m => m ()
refsLocal = streamCall _refsLocal

-- | Send echo request packets to IPFS hosts.
ping :: MonadIO m => Text -> m ()
ping = streamCall . _ping

-- | Shut down the ipfs daemon.
shutdown :: MonadIO m => IpfsT m NoContent
shutdown = call _shutdown
