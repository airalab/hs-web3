-- |
-- Module      :  Network.Ethereum.Web3.Types
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  MIT
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unknown
--
-- Web3 types and instances
--
module Network.Ethereum.Web3.Types where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Data.Default.Class

data Config = Config
  { rpcUri :: String
  } deriving (Show, Eq)

instance Default Config where
    def = Config "http://localhost:8545"

data Error = Error
  { rpcError :: Maybe String
  } deriving (Show, Eq)

instance Default Error where
    def = Error Nothing

type Web3 = ReaderT Config (ExceptT Error IO)

-- | Run 'Web3' monad with default config.
runWeb3 :: MonadIO m => Web3 a -> m (Either Error a)
runWeb3 = runWeb3' def

-- | Run 'Web3' monad.
runWeb3' :: MonadIO m => Config -> Web3 a -> m (Either Error a)
runWeb3' c = liftIO . runExceptT . flip runReaderT c
