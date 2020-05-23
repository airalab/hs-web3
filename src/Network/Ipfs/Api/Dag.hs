{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Ipfs.Api.Dag
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unknown
--
-- Api calls with `dag` prefix.
--

module Network.Ipfs.Api.Dag where

import           Control.Monad.IO.Class         (MonadIO)
import           Data.Aeson                     (decode)
import           Data.Text                      (Text)
import           Network.HTTP.Client            (responseBody)

import           Network.Ipfs.Api.Internal      (_dagGet, _dagResolve)
import           Network.Ipfs.Api.Internal.Call (call, multipartCall)
import           Network.Ipfs.Api.Types         (DagPutObj, DagResolveObj,
                                                 DagReturnType)
import           Network.Ipfs.Client            (IpfsT)

-- | Get a dag node from ipfs.
get :: MonadIO m => Text -> IpfsT m DagReturnType
get = call . _dagGet

-- | Resolve ipld block.
resolve :: MonadIO m => Text -> IpfsT m DagResolveObj
resolve = call . _dagResolve

-- | Add a dag node to ipfs.
put :: MonadIO m => Text -> IpfsT m (Maybe DagPutObj)
put = fmap decodeResponse . multipartCall "dag/put"
  where
    decodeResponse = decode . responseBody
