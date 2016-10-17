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

import Network.Ethereum.Web3.Internal (toLowerFirst)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Data.Default.Class
import Data.Text (Text)
import Data.Aeson.TH
import Data.Aeson

-- | Main monad type
type Web3 = ReaderT Config (ExceptT Error IO)

-- | Web3 configuration
data Config = Config
  { rpcUri :: String
  -- ^ JSON-RPC node URI
  } deriving (Show, Eq)

instance Default Config where
    def = Config "http://localhost:8545"

data Error = JsonRpcFail RpcError
           | ParserFail String
  deriving (Show, Eq)

-- | JSON-RPC error.
data RpcError = RpcError
  { errCode     :: Int
  , errMessage  :: Text
  , errData     :: Maybe Value
  } deriving (Show, Eq)

$(deriveJSON (defaultOptions
    { fieldLabelModifier = toLowerFirst . drop 3 }) ''RpcError)

data Filter = Filter
  { filterAddress   :: Maybe Text
  , filterTopics    :: Maybe [Maybe Text]
  , filterFromBlock :: Maybe Text
  , filterToBlock   :: Maybe Text
  } deriving Show

$(deriveJSON (defaultOptions
    { fieldLabelModifier = toLowerFirst . drop 6 }) ''Filter)

data Change = Change
  { changeLogIndex         :: Text
  , changeTransactionIndex :: Text
  , changeTransactionHash  :: Text
  , changeBlockHash        :: Text
  , changeBlockNumber      :: Text
  , changeAddress          :: Text
  , changeData             :: Text
  , changeTopics           :: [Text]
  } deriving Show

$(deriveJSON (defaultOptions
    { fieldLabelModifier = toLowerFirst . drop 6 }) ''Change)

data Call = Call
  { callFrom    :: Maybe Text
  , callTo      :: Text
  , callGas     :: Maybe Text
  , callPrice   :: Maybe Text
  , callValue   :: Maybe Text
  , callData    :: Maybe Text
  } deriving Show

$(deriveJSON (defaultOptions
    { fieldLabelModifier = toLowerFirst . drop 4 }) ''Call)

