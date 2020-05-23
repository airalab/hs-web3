{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Ipfs.Api.Config
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unknown
--
-- Api calls with `config` prefix.
--

module Network.Ipfs.Api.Config where

import           Control.Monad.IO.Class         (MonadIO)
import           Data.Text                      (Text)
import           Network.HTTP.Client            (responseStatus)
import           Network.HTTP.Types             (Status (..))

import           Network.Ipfs.Api.Internal      (_configGet, _configSet)
import           Network.Ipfs.Api.Internal.Call (call, multipartCall)
import           Network.Ipfs.Api.Types         (ConfigObj)
import           Network.Ipfs.Client            (IpfsT)


-- | Get ipfs config values.
get :: MonadIO m => Text -> IpfsT m ConfigObj
get = call . _configGet

-- | Set ipfs config values.
set :: MonadIO m => Text -> Maybe Text -> IpfsT m ConfigObj
set key = call . _configSet key

-- | Replace the config with the file at <filePath>.
replace :: MonadIO m => Text -> IpfsT m Bool
replace = fmap isSuccess . multipartCall "config/replace"
  where
    isSuccess = (== 200) . statusCode . responseStatus
