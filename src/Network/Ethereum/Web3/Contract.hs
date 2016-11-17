module Network.Ethereum.Web3.Contract where

import Network.Ethereum.Web3.Encoding (ABIEncoding)
import Network.Ethereum.Address (Address)
import Control.Monad.IO.Class (MonadIO)

class ABIEncoding a => Event a where
    event :: MonadIO m => Address -> (a -> m ()) -> m ()

class ABIEncoding a => Method a where
    call :: MonadIO m => Address -> a -> m ()
