{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

-- |
-- Module      :  Network.Ethereum.Web3.Provider
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Web3 service provider.
--

module Network.Ethereum.Web3.Provider where

import           Control.Concurrent.Async   (Async, async)
import           Control.Exception          (Exception, try)
import           Control.Monad.Catch        (MonadThrow)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.State        (MonadState (..))
import           Control.Monad.Trans.State  (StateT, evalStateT)
import           Data.Default               (Default (..))
import           GHC.Generics               (Generic)
import           Lens.Micro.Mtl             ((.=))
import           Network.HTTP.Client        (Manager)

import           Network.JsonRpc.TinyClient (JsonRpcClient, defaultSettings,
                                             jsonRpcManager)

-- | Any communication with Ethereum node wrapped with 'Web3' monad
newtype Web3 a = Web3 { unWeb3 :: StateT JsonRpcClient IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow)

instance MonadState JsonRpcClient Web3 where
    get = Web3 get
    put = Web3 . put
    state = Web3 . state

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
data Provider = HttpProvider String
  deriving (Show, Eq, Generic)

instance Default Provider where
  def = HttpProvider "http://localhost:8545"

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

-- | 'Web3' runner for default provider
runWeb3 :: MonadIO m
        => Web3 a
        -> m (Either Web3Error a)
{-# INLINE runWeb3 #-}
runWeb3 = runWeb3' def

-- | Fork 'Web3' with the same 'Provider' and 'Manager'
forkWeb3 :: Web3 a -> Web3 (Async a)
forkWeb3 f = liftIO . async . evalStateT (unWeb3 f) =<< get
