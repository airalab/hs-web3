{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module      :  Network.Ethereum.Web3.Types
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  MIT
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unknown
--
-- Common used types and instances
--
module Network.Ethereum.Web3.Types where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Data.Char (toLower)
import Data.Default.Class
import Data.Text (Text)
import Data.Aeson.TH
import Data.Aeson

-- | JSON-RPC error.
data RpcError = RpcError
  { errCode     :: Int
  , errMessage  :: Text
  , errData     :: Maybe Value
  } deriving (Show, Eq)

$(deriveJSON (defaultOptions { fieldLabelModifier =
    let f (x : xs) = toLower x : xs in f . drop 3 }) ''RpcError)

data Config = Config
  { rpcUri :: String
  } deriving (Show, Eq)

instance Default Config where
    def = Config "http://localhost:8545"

data Error = JsonRpcFail RpcError
           | ParserFail String
  deriving (Show, Eq)

-- | Main monad type
type Web3 = ReaderT Config (ExceptT Error IO)

-- | Run 'Web3' monad with default config.
runWeb3 :: MonadIO m => Web3 a -> m (Either Error a)
runWeb3 = runWeb3' def

-- | Run 'Web3' monad.
runWeb3' :: MonadIO m => Config -> Web3 a -> m (Either Error a)
runWeb3' c = liftIO . runExceptT . flip runReaderT c
