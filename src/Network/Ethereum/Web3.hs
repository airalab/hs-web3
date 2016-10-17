module Network.Ethereum.Web3 (
    Web3
  , Error(..)
  , Config(..)
  , runWeb3
  , runWeb3'
  , module Network.Ethereum.Web3.Util
  , module Network.Ethereum.Web3.API
  ) where

import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Reader (runReaderT)
import Network.Ethereum.Web3.Types
import Network.Ethereum.Web3.Util
import Network.Ethereum.Web3.API
import Data.Default.Class (def)
import Control.Monad.IO.Class

-- | Run 'Web3' monad with default config.
runWeb3 :: MonadIO m => Web3 a -> m (Either Error a)
runWeb3 = runWeb3' def

-- | Run 'Web3' monad.
runWeb3' :: MonadIO m => Config -> Web3 a -> m (Either Error a)
runWeb3' c = liftIO . runExceptT . flip runReaderT c
