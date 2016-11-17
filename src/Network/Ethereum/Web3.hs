-- |
-- Module      :  Network.Ethereum.Web3
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unknown
--
-- Web3 main module.
--
module Network.Ethereum.Web3 (
    module Network.Ethereum.Web3.Contract
  , Config(..)
  , Error(..)
  , runWeb3'
  , runWeb3
  , Web3
  ) where

import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.IO.Class (MonadIO(..))
import Network.Ethereum.Web3.Contract
import Network.Ethereum.Web3.Types
import Network.Ethereum.Web3.Api
import Data.Default.Class (def)

-- | Run 'Web3' monad with default config.
runWeb3 :: MonadIO m => Web3 a -> m (Either Error a)
runWeb3 = runWeb3' def

-- | Run 'Web3' monad.
runWeb3' :: MonadIO m => Config -> Web3 a -> m (Either Error a)
runWeb3' c = liftIO . runExceptT . flip runReaderT c
