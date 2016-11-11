{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module      :  Network.Ethereum.Web3.Types
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  MIT
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Common used types and instances.
--
module Network.Ethereum.Web3.Types where

import Network.Ethereum.Web3.Internal (toLowerFirst)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Except (ExceptT)
import Network.Ethereum.Address (Address)
import Data.Default.Class (Default(..))
import Data.Text (Text)
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
           | ParserFail  String
           | UserFail    String
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
  { filterAddress   :: Maybe Address
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
  , changeAddress          :: Address
  , changeData             :: Text
  , changeTopics           :: [Text]
  } deriving Show

$(deriveJSON (defaultOptions
    { fieldLabelModifier = toLowerFirst . drop 6 }) ''Change)

data Call = Call
  { callFrom    :: Maybe Address
  , callTo      :: Address
  , callGas     :: Maybe Text
  , callPrice   :: Maybe Text
  , callValue   :: Maybe Text
  , callData    :: Maybe Text
  } deriving Show

$(deriveJSON (defaultOptions
    { fieldLabelModifier = toLowerFirst . drop 4 }) ''Call)
