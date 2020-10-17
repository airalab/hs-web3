-- |
-- Module      :  Network.Ipfs.Api.Pin
-- Copyright   :  Aleksandr Krupenkin 2016-2020
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unknown
--
-- Api calls with `pin` prefix.
--

module Network.Ipfs.Api.Pin where

import           Control.Monad.IO.Class         (MonadIO)
import           Data.Text                      (Text)

import           Network.Ipfs.Api.Internal      (_pinAdd, _pinRemove)
import           Network.Ipfs.Api.Internal.Call (call)
import           Network.Ipfs.Api.Types         (PinObj)
import           Network.Ipfs.Client            (IpfsT)

-- | Pin objects to local storage.
add :: MonadIO m => Text -> IpfsT m PinObj
add = call . _pinAdd

-- | Remove pinned objects from local storage.
remove :: MonadIO m => Text -> IpfsT m PinObj
remove = call . _pinRemove
