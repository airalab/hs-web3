{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

-- |
-- Module      :  Network.Ethereum.Api.Provider
-- Copyright   :  Alexander Krupenkin 2016-2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Web3 service provider.
--

module Network.Ethereum.Api.Provider where

import           Control.Concurrent.Async   (Async, async)
import           Control.Exception          (Exception, try)
import           Control.Monad.Catch        (MonadThrow)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.State        (MonadState (..))
import           Control.Monad.Trans.State  (StateT, evalStateT)
import           GHC.Generics               (Generic)
import           Lens.Micro.Mtl             ((.=))
import           Network.HTTP.Client        (Manager)

import           Network.JsonRpc.TinyClient (JsonRpc, JsonRpcClient,
                                             defaultSettings, jsonRpcManager)

-- | Any communication with Ethereum node wrapped with 'Web3' monad
newtype Web3 a = Web3 { unWeb3 :: StateT JsonRpcClient IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadState JsonRpcClient)

instance JsonRpc Web3

-- | Some peace of error response
data Web3Error
  = JsonRpcFail !String
  -- ^ JSON-RPC communication error
  | ParserFail  !String
  -- ^ Error in parser state
  | UserFail    !String
  -- ^ Common head for user errors
  deriving (Show, Eq, Generic)

instance Exception Web3Error

--TODO: Change to `HttpProvider ServerUri | IpcProvider FilePath` to support IPC
-- | Web3 Provider
data Provider = HttpProvider String | WSProvider String
  deriving (Show, Eq, Generic)

defaultHttpPovider = HttpProvider "http://localhost:8545"
defaultWSPovider   = WSProvider   "ws://127.0.0.1:8546"

-- | 'Web3' monad runner, using the supplied Manager
runWeb3With :: MonadIO m
            => Manager
            -> Provider
            -> Web3 a
            -> m (Either Web3Error a)
runWeb3With manager provider f =
    runWeb3' provider $ jsonRpcManager .= manager >> f

-- | 'Web3' monad runner
runWeb3' :: MonadIO m
         => Provider
         -> Web3 a
         -> m (Either Web3Error a)
runWeb3' (HttpProvider uri) f = do
    cfg <- defaultSettings uri
    liftIO . try . flip evalStateT cfg . unWeb3 $ f

-- | 'Web3' runner for default http provider
runWeb3 :: MonadIO m
        => Web3 a
        -> m (Either Web3Error a)
{-# INLINE runWeb3 #-}
runWeb3 = runWeb3' defaultHttpPovider

-- | 'Web3' runner for default WS provider
runWeb3WS :: MonadIO m
        => Web3 a
        -> m (Either Web3Error a)
{-# INLINE runWeb3WS #-}
runWeb3WS = runWeb3' defaultWSPovider

-- | Fork 'Web3' with the same 'Provider' and 'Manager'
forkWeb3 :: Web3 a -> Web3 (Async a)
forkWeb3 f = liftIO . async . evalStateT (unWeb3 f) =<< get
