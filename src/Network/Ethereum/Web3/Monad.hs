{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Network.Ethereum.Web3.Monad where

import           Control.Exception          (Exception)
import           Control.Monad.Catch        (MonadThrow)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Reader       (MonadReader (..))
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Aeson                 (FromJSON)
import           Data.Default               (Default (..))
import           Data.Typeable              (Typeable)
import           GHC.Generics               (Generic)
import           Network.HTTP.Client        (Manager)
import           Network.JsonRpc.TinyClient (Remote, ServerUri)

-- | Any communication with Ethereum node wrapped with 'Web3' monad
newtype Web3 a = Web3 { unWeb3 :: ReaderT (ServerUri, Manager) IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow)

instance MonadReader (ServerUri, Manager) Web3 where
    ask = Web3 ask
    local f = Web3 . local f . unWeb3

instance FromJSON a => Remote Web3 (Web3 a)

-- | Some peace of error response
data Web3Error
  = JsonRpcFail !String
  -- ^ JSON-RPC communication error
  | ParserFail  !String
  -- ^ Error in parser state
  | UserFail    !String
  -- ^ Common head for user errors
  deriving (Typeable, Show, Eq, Generic)

instance Exception Web3Error
