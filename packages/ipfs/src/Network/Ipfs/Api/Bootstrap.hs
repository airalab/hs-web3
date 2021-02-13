-- |
-- Module      :  Network.Ipfs.Api.Bootstrap
-- Copyright   :  Aleksandr Krupenkin 2016-2021
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unknown
--
-- Api calls with `bootstrap` prefix.
--

module Network.Ipfs.Api.Bootstrap where

import           Control.Monad.IO.Class         (MonadIO)
import           Data.Text                      (Text)

import           Network.Ipfs.Api.Internal      (_bootstrapAdd, _bootstrapList,
                                                 _bootstrapRM)
import           Network.Ipfs.Api.Internal.Call (call)
import           Network.Ipfs.Api.Types         (BootstrapObj)
import           Network.Ipfs.Client            (IpfsT)

-- | Add peers to the bootstrap list.
add :: MonadIO m => Text -> IpfsT m BootstrapObj
add = call . _bootstrapAdd . Just

-- | Show peers in the bootstrap list.
list :: MonadIO m => IpfsT m BootstrapObj
list = call _bootstrapList

-- | Remove peers from the bootstrap list.
rm :: MonadIO m => Text -> IpfsT m BootstrapObj
rm = call . _bootstrapRM . Just
