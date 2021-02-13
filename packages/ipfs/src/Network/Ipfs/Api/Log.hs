{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

-- |
-- Module      :  Network.Ipfs.Api.Log
-- Copyright   :  Aleksandr Krupenkin 2016-2021
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unknown
--
-- Api calls with `log` prefix.
--

module Network.Ipfs.Api.Log where

import           Control.Monad.IO.Class           (MonadIO)
import           Data.Text                        (Text)

import           Network.Ipfs.Api.Internal        (_logLevel, _logLs)
import           Network.Ipfs.Api.Internal.Call   (call, streamCall)
import           Network.Ipfs.Api.Internal.Stream (_logTail)
import           Network.Ipfs.Api.Types           (LogLevelObj, LogLsObj)
import           Network.Ipfs.Client              (IpfsT)


-- | Change the logging level.
level :: MonadIO m => Text -> Text -> IpfsT m LogLevelObj
level subsystem = call . _logLevel subsystem . Just

-- | Read the event log.
tail :: MonadIO m => m ()
tail = streamCall _logTail

-- | List the logging subsystems.
ls :: MonadIO m => IpfsT m LogLsObj
ls = call _logLs
