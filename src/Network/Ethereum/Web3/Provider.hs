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
import           Data.Default (def)
import           Network.Ethereum.Web3.Types
import           Network.HTTP.Client         (newManager, Manager)
import           Network.HTTP.Client.TLS     (tlsManagerSettings)

-- | 'Web3' monad runner, using the supplied Manager
runWeb3With :: MonadIO m => Manager -> Provider -> Web3 a -> m (Either Web3Error a)
{-# INLINE runWeb3With #-}
runWeb3With manager (HttpProvider uri) f =
    liftIO . try .  flip runReaderT (uri, manager) . unWeb3 $ f

-- | 'Web3' monad runner
runWeb3' :: MonadIO m => Provider -> Web3 a -> m (Either Web3Error a)
{-# INLINE runWeb3' #-}
runWeb3' provider f = do
    manager <- liftIO $ newManager tlsManagerSettings
    runWeb3With manager provider f

-- | 'Web3' runner for default provider
runWeb3 :: MonadIO m => Web3 a -> m (Either Web3Error a)
{-# INLINE runWeb3 #-}
runWeb3 = runWeb3' def

-- | Fork 'Web3' with the same 'Provider'
forkWeb3 :: Web3 a -> Web3 (Async a)
{-# INLINE forkWeb3 #-}
forkWeb3 =   Web3 . mapReaderT async . unWeb3


