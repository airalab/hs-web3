{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module      :  Network.Ethereum.Web3.Internal
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  MIT
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unknown
--
-- Web3 API methods.
--
module Network.Ethereum.Web3.Internal where

import Control.Monad.Error.Class (throwError)
import Network.Ethereum.Web3.JsonRpc
import Data.Text.Read (hexadecimal)
import Network.Ethereum.Web3.Types
import Data.Aeson (Value, toJSON)
import Control.Monad.Trans (lift)
import Data.Default.Class (def)
import Control.Monad ((>=>))
import Data.Char (toLower)
import Data.Text (Text)
import Data.Aeson.TH

web3_clientVersion :: Web3 Text
web3_clientVersion = remote "web3_clientVersion"

web3_sha3 :: Text -> Web3 Text
web3_sha3 = remote "web3_sha3"

eth_getBalance :: Text -> Text -> Web3 Text
eth_getBalance = remote "eth_getBalance"

data Filter = Filter
  { filterAddress   :: Maybe Text
  , filterTopics    :: Maybe [Maybe Text]
  , filterFromBlock :: Maybe Text
  , filterToBlock   :: Maybe Text
  } deriving Show

$(deriveJSON (defaultOptions { fieldLabelModifier =
    let f (x : xs) = toLower x : xs in f . drop 6 }) ''Filter)

-- | Creates a filter object, based on filter options, to notify when the
-- state changes (logs). To check if the state has changed, call
-- 'getFilterChanges'.
eth_newFilter :: Filter -> Web3 Text
eth_newFilter = remote "eth_newFilter"

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

$(deriveJSON (defaultOptions { fieldLabelModifier =
    let f (x : xs) = toLower x : xs in f . drop 6 }) ''Change)

-- | Polling method for a filter, which returns an array of logs which
-- occurred since last poll.
eth_getFilterChanges :: Text -> Web3 [Change]
eth_getFilterChanges = remote "eth_getFilterChanges"

data Call = Call
  { callFrom    :: Maybe Text
  , callTo      :: Text
  , callGas     :: Maybe Text
  , callPrice   :: Maybe Text
  , callValue   :: Maybe Text
  , callData    :: Maybe Text
  } deriving Show

$(deriveJSON (defaultOptions { fieldLabelModifier =
    let f (x : xs) = toLower x : xs in f . drop 4 }) ''Call)

eth_call :: Call -> Text -> Web3 Text
eth_call = remote "eth_call"

eth_sendTransaction :: Call -> Web3 Text
eth_sendTransaction = remote "eth_sendTransaction"
