{-# LANGUAGE CPP                        #-}
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

import           Control.Concurrent.Async      (Async, async)
import           Control.Exception             (Exception, try)
import           Control.Monad.Catch           (MonadThrow)
import           Control.Monad.IO.Class        (MonadIO (..))
import           Control.Monad.Reader          (MonadReader (..))
import           Control.Monad.Trans.Reader    (ReaderT, mapReaderT, runReaderT)
import           Data.ByteString               (ByteString)
import           Data.Default                  (Default (..))
import           GHC.Generics                  (Generic)
import           Network.Ethereum.Web3.Logging
import           Network.HTTP.Client           (Manager, newManager)

#ifdef TLS_MANAGER
import           Network.HTTP.Client.TLS       (tlsManagerSettings)
#else
import           Network.HTTP.Client           (defaultManagerSettings)
#endif

-- | Any communication with Ethereum node wrapped with 'Web3' monad
newtype Web3 a = Web3 { unWeb3 :: ReaderT (Provider, Manager, Web3Logger) IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow)

instance MonadReader (Provider, Manager, Web3Logger) Web3 where
    ask = Web3 ask
    local f = Web3 . local f . unWeb3

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

-- | JSON-RPC server URI
type ServerUri  = String

--TODO: Change to `HttpProvider ServerUri | IpcProvider FilePath` to support IPC
data JsonRpcProvider = HttpProvider ServerUri
  deriving (Show, Eq, Generic)

-- | Web3 Provider
data Provider = Provider { jsonRpc              :: JsonRpcProvider
                         , signingConfiguration :: Maybe SigningConfiguration
                         } deriving (Show, Eq)

data SigningConfiguration = SigningConfiguration { privateKey      :: ByteString
                                                 , chainIdentifier :: Integer } deriving (Show, Eq)

instance Default Provider where
  def = Provider (HttpProvider "http://localhost:8545") Nothing

-- | 'Web3' monad runner, using the supplied Manager
runWeb3With :: MonadIO m => Manager -> Provider -> Web3 a -> m (Either Web3Error a)
runWeb3With manager provider = runWeb3With' manager provider noopLogger

runWeb3With' :: MonadIO m => Manager -> Provider -> Web3Logger -> Web3 a -> m (Either Web3Error a)
runWeb3With' manager provider w3logger =
    liftIO . try .  flip runReaderT (provider, manager, w3logger) . unWeb3

-- | 'Web3' monad runner
runWeb3' :: MonadIO m => Provider -> Web3 a -> m (Either Web3Error a)
runWeb3' provider f = do
    manager <- liftIO $
#ifdef TLS_MANAGER
        newManager tlsManagerSettings
#else
        newManager defaultManagerSettings
#endif
    runWeb3With manager provider f

-- | 'Web3' runner for default provider
runWeb3 :: MonadIO m => Web3 a -> m (Either Web3Error a)
{-# INLINE runWeb3 #-}
runWeb3 = runWeb3' def

-- | Fork 'Web3' with the same 'Provider'
forkWeb3 :: Web3 a -> Web3 (Async a)
{-# INLINE forkWeb3 #-}
forkWeb3 = Web3 . mapReaderT async . unWeb3

web3Trace :: String -> Web3 ()
web3Trace msg = do
    (_, _, logger) <- ask
    liftIO . unWeb3Logger logger $ W3LMTrace msg
