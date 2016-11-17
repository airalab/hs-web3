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
import Data.Text (Text)

web3_clientVersion :: Web3 Text
web3_clientVersion = remote "web3_clientVersion"

web3_sha3 :: Text -> Web3 Text
web3_sha3 = remote "web3_sha3"

eth_getBalance :: Text -> Text -> Web3 Text
eth_getBalance = remote "eth_getBalance"

-- | Creates a filter object, based on filter options, to notify when the
-- state changes (logs). To check if the state has changed, call
-- 'getFilterChanges'.
eth_newFilter :: Filter -> Web3 Text
eth_newFilter = remote "eth_newFilter"

-- | Polling method for a filter, which returns an array of logs which
-- occurred since last poll.
eth_getFilterChanges :: Text -> Web3 [Change]
eth_getFilterChanges = remote "eth_getFilterChanges"

eth_call :: Call -> Text -> Web3 Text
eth_call = remote "eth_call"

eth_sendTransaction :: Call -> Web3 Text
eth_sendTransaction = remote "eth_sendTransaction"
