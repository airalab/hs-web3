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
import           Network.Ethereum.Web3.Types

-- | Ethereum node service provider
class Provider a where
    -- | JSON-RPC provider URI, default: localhost:8545
    rpcUri :: Web3 a String

-- | Default 'Web3' service provider
data DefaultProvider

instance Provider DefaultProvider where
    rpcUri = return "http://localhost:8545"

-- | 'Web3' monad runner
runWeb3' :: MonadIO m => Web3 a b -> m (Either Web3Error b)
{-# INLINE runWeb3' #-}
runWeb3' = liftIO . try . unWeb3

-- | 'Web3' runner for default provider
runWeb3 :: MonadIO m => Web3 DefaultProvider b -> m (Either Web3Error b)
{-# INLINE runWeb3 #-}
runWeb3 = runWeb3'

-- | Fork 'Web3' with the same 'Provider'
forkWeb3 :: Web3 a b -> Web3 a (Async b)
{-# INLINE forkWeb3 #-}
forkWeb3 = Web3 . async . unWeb3
