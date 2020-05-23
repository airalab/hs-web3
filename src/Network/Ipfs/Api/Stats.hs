-- |
-- Module      :  Network.Ipfs.Api.Stats
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unknown
--
-- Api calls with `stats` prefix.
--

module Network.Ipfs.Api.Stats where

import           Control.Monad.IO.Class         (MonadIO)

import           Network.Ipfs.Api.Internal      (_statsBw, _statsRepo)
import           Network.Ipfs.Api.Internal.Call (call)
import           Network.Ipfs.Api.Types         (StatsBwObj, StatsRepoObj)
import           Network.Ipfs.Client            (IpfsT)

-- | IPFS bandwidth information.
bw :: MonadIO m => IpfsT m StatsBwObj
bw = call _statsBw

-- | Get stats for the currently used repo.
repo :: MonadIO m => IpfsT m StatsRepoObj
repo = call _statsRepo
