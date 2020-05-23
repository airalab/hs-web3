{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Ipfs.Api.Block
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unknown
--
-- Api calls with `block` prefix.
--

module Network.Ipfs.Api.Block where

import           Control.Monad.IO.Class         (MonadIO)
import           Data.Aeson                     (decode)
import           Data.Text                      (Text)
import           Network.HTTP.Client            (responseBody)

import           Network.Ipfs.Api.Internal      (_blockGet, _blockStat)
import           Network.Ipfs.Api.Internal.Call (call, multipartCall)
import           Network.Ipfs.Api.Types         (BlockObj, BlockReturnType)
import           Network.Ipfs.Client            (IpfsT)

-- | Get a raw IPFS block.
get :: MonadIO m => Text -> IpfsT m BlockReturnType
get = call . _blockGet

-- | Store input as an IPFS block.
put :: MonadIO m => Text -> IpfsT m (Maybe BlockObj)
put = fmap decodeResponse . multipartCall "block/put"
  where
    decodeResponse = decode . responseBody

-- | Print information of a raw IPFS block.
stat :: MonadIO m => Text -> IpfsT m BlockObj
stat = call . _blockStat
