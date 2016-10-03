{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module      :  Network.Ethereum.Web3.Api
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  MIT
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unknown
--
-- Web3 API methods.
--
module Network.Ethereum.Web3.Api where

import Network.Ethereum.Web3.JsonRpc
import Network.Ethereum.Web3.Types
import Data.Char (toLower)
import Data.Aeson (Value)
import Data.Text (Text)
import Data.Aeson.TH

clientVersion :: Web3 Text
clientVersion = remote "web3_clientVersion"

sha3 :: Text -> Web3 Text
sha3 = remote "web3_sha3"

getBalance :: Text -> Text -> Web3 Text
getBalance = remote "eth_getBalance"

data Filter = Filter
  { filterAddress   :: Maybe Text
  , filterTopics    :: Maybe Value
  , filterFromBlock :: Maybe Text
  , filterToBlock   :: Maybe Text
  } deriving Show

$(deriveJSON (defaultOptions { fieldLabelModifier =
    let f (x : xs) = toLower x : xs in f . drop 6 }) ''Filter)

-- | Creates a filter object, based on filter options, to notify when the
-- state changes (logs). To check if the state has changed, call
-- 'getFilterChanges'.
newFilter :: Filter -> Web3 Text
newFilter = remote "eth_newFilter"

data Update = Update
  { updateLogIndex         :: Text
  , updateTransactionIndex :: Text
  , updateTransactionHash  :: Text
  , updateBlockHash        :: Text
  , updateBlockNumber      :: Text
  , updateAddress          :: Text
  , updateData             :: Text
  , updateTopics           :: [Text]
  } deriving Show

$(deriveJSON (defaultOptions { fieldLabelModifier =
    let f (x : xs) = toLower x : xs in f . drop 6 }) ''Update)

-- | Polling method for a filter, which returns an array of logs which
-- occurred since last poll.
getFilterChanges :: Text -> Web3 [Update]
getFilterChanges = remote "eth_getFilterChanges"
