-- |
-- Module      :  Network.Ipfs.Api.Bitswap
-- Copyright   :  Aleksandr Krupenkin 2016-2020
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unknown
--
-- Api calls with `bitswap` prefix.
--

module Network.Ipfs.Api.Bitswap where

import           Control.Monad.IO.Class         (MonadIO (..))
import           Data.Text                      (Text)

import           Network.Ipfs.Api.Internal      (_bitswapLedger,
                                                 _bitswapReprovide,
                                                 _bitswapStat, _bitswapWL)
import           Network.Ipfs.Api.Internal.Call (call)
import           Network.Ipfs.Api.Types         (BitswapLedgerObj,
                                                 BitswapStatObj, BitswapWLObj,
                                                 ReprovideReturnType)
import           Network.Ipfs.Client            (IpfsT)

-- | 'Show some diagnostic information on the bitswap agent.
stat :: MonadIO m => IpfsT m BitswapStatObj
stat = call _bitswapStat

-- | Show blocks currently on the wantlist.
wl :: MonadIO m => IpfsT m BitswapWLObj
wl = call _bitswapWL

-- | Show the current ledger for a peer.
ledger :: MonadIO m => Text -> IpfsT m BitswapLedgerObj
ledger = call . _bitswapLedger

-- | Trigger reprovider.
reprovide :: MonadIO m => IpfsT m ReprovideReturnType
reprovide = call _bitswapReprovide
