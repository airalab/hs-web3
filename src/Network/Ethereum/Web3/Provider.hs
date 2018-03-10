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

import           Control.Concurrent.Async    (Async, async)
import           Control.Exception           (try)
import           Control.Monad.IO.Class      (MonadIO (..))
import           Control.Monad.Trans.Reader  (mapReaderT, runReaderT)
import           Network.Ethereum.Web3.Types
import           Network.HTTP.Client         (newManager)
import           Network.HTTP.Client.TLS     (tlsManagerSettings)

-- | 'Web3' monad runner
runWeb3' :: MonadIO m => ServerUri -> Web3 a -> m (Either Web3Error a)
{-# INLINE runWeb3' #-}
runWeb3' uri f = do
    manager <- liftIO $ newManager tlsManagerSettings
    liftIO . try .  flip runReaderT (uri, manager) . unWeb3 $ f

-- | 'Web3' runner for default provider
runWeb3 :: MonadIO m => Web3 a -> m (Either Web3Error a)
{-# INLINE runWeb3 #-}
runWeb3 = runWeb3' "http://localhost:8545"

-- | Fork 'Web3' with the same 'Provider'
forkWeb3 :: Web3 a -> Web3 (Async a)
{-# INLINE forkWeb3 #-}
forkWeb3 =   Web3 . mapReaderT async . unWeb3
