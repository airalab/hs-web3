{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

-- |
-- Module      :  Network.Web3.Provider
-- Copyright   :  Aleksandr Krupenkin 2016-2021
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Web3 service provider.
--

module Network.Web3.Provider where

import           Control.Concurrent.Async   (Async, async)
import           Control.Exception          (Exception, try)
import           Control.Monad.Catch        (MonadThrow)
import           Control.Monad.Fail         (MonadFail)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.State        (MonadState (..))
import           Control.Monad.Trans.State  (StateT, evalStateT, withStateT)
import           Data.Default               (Default (..))
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)
import           Network.HTTP.Client        (Manager)
import           Network.JsonRpc.TinyClient (JsonRpc, JsonRpcClient (..),
                                             defaultSettings, jsonRpcManager)
import qualified Network.Socket             as S
import qualified Network.WebSockets         as WS (Connection,
                                                   defaultConnectionOptions,
                                                   newClientConnection,
                                                   sendClose)
import qualified Network.WebSockets.Stream  as Stream

-- | Any communication with node wrapped with 'Web3' monad
newtype Web3 a = Web3 { unWeb3 :: StateT JsonRpcClient IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadFail, MonadState JsonRpcClient)

instance JsonRpc Web3

-- | Some peace of error response
data Web3Error = JsonRpcFail !String
    | ParserFail !String
    | UserFail !String
    deriving (Show, Eq, Generic)

instance Exception Web3Error

--TODO: Change to `HttpProvider ServerUri | IpcProvider FilePath` to support IPC
-- | Web3 Provider
data Provider = HttpProvider String
    | WsProvider String Int
    deriving (Show, Eq, Generic)

-- | Default Provider URI
instance Default Provider where
  def = HttpProvider "http://localhost:8545"

-- | 'Web3' monad runner, using the supplied Manager
runWeb3With :: MonadIO m
            => Manager
            -> Provider
            -> Web3 a
            -> m (Either Web3Error a)
runWeb3With manager provider f = do
    runWeb3' provider Web3 { unWeb3 = withStateT changeManager $ unWeb3 f}
    where
      changeManager jRpcClient = case jRpcClient of
        JsonRpcHttpClient{..} -> jRpcClient { jsonRpcManager = manager }
        JsonRpcWsClient{..}   -> jRpcClient

-- | 'Web3' monad runner
runWeb3' :: MonadIO m
         => Provider
         -> Web3 a
         -> m (Either Web3Error a)
runWeb3' (HttpProvider uri) f = do
    cfg <- defaultSettings uri
    liftIO . try . flip evalStateT cfg . unWeb3 $ f

runWeb3' (WsProvider host port) f = do
    connection <- liftIO $ getConnection host port "/"
    let currentClient = JsonRpcWsClient { jsonRpcWsConnection = connection }
    response <- liftIO $ try . flip evalStateT currentClient . unWeb3 $ f
    liftIO $ WS.sendClose connection ("Bye-" :: Text)
    return response

-- | 'Web3' runner for default Http provider
runWeb3 :: MonadIO m
        => Web3 a
        -> m (Either Web3Error a)
{-# INLINE runWeb3 #-}
runWeb3 = runWeb3' def

-- | Fork 'Web3' with the same 'Provider' and 'Manager'
forkWeb3 :: Web3 a -> Web3 (Async a)
forkWeb3 f = liftIO . async . evalStateT (unWeb3 f) =<< get

-- | Returns a WebSocket Connection Instance
getConnection :: String           -- ^ Host
              -> Int              -- ^ Port
              -> String           -- ^ Path
              -> IO WS.Connection
{-# INLINE getConnection #-}
getConnection host port path = do
    -- Create and connect socket
    let hints = S.defaultHints
                    {S.addrSocketType = S.Stream}

        -- Correct host and path.
        fullHost = if port == 80 then host else (host ++ ":" ++ show port)
        path0     = if null path then "/" else path

    addr:_ <- S.getAddrInfo (Just hints) (Just host) (Just $ show port)
    sock      <- S.socket (S.addrFamily addr) S.Stream S.defaultProtocol
    S.setSocketOption sock S.NoDelay 1

    -- Connect WebSocket and run client

    res <-  ( S.connect sock (S.addrAddress addr) >>
            Stream.makeSocketStream sock) >>=
              (\stream ->
                    WS.newClientConnection stream fullHost
                    path0 WS.defaultConnectionOptions [] )
    return res
