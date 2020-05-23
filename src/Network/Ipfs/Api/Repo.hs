-- |
-- Module      :  Network.Ipfs.Api.Repo
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unknown
--
-- Api calls with `repo` prefix.
--

module Network.Ipfs.Api.Repo where

import           Control.Monad.IO.Class           (MonadIO)

import           Network.Ipfs.Api.Internal        (_repoFsck, _repoVersion)
import           Network.Ipfs.Api.Internal.Call   (call, streamCall)
import           Network.Ipfs.Api.Internal.Stream (_repoGc, _repoVerify)
import           Network.Ipfs.Api.Types           (RepoFsckObj, RepoVersionObj)
import           Network.Ipfs.Client              (IpfsT)

-- | Show the repo version.
version :: MonadIO m => IpfsT m RepoVersionObj
version = call _repoVersion

-- | Remove repo lockfiles.
fsck :: MonadIO m => IpfsT m RepoFsckObj
fsck = call _repoFsck

-- | Perform a garbage collection sweep on the repo.
gc :: MonadIO m => m ()
gc = streamCall _repoGc

-- | Verify all blocks in repo are not corrupted.
repoVerify :: MonadIO m => m ()
repoVerify = streamCall _repoVerify
