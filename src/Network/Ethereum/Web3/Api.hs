-- |
-- Module      :  Network.Ethereum.Web3.Api
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unknown
--
-- Ethereum node JSON-RPC API methods.
--
module Network.Ethereum.Web3.Api where

import Network.Ethereum.Web3.Provider
import Network.Ethereum.Web3.Address
import Network.Ethereum.Web3.JsonRpc
import Network.Ethereum.Web3.Types
import Data.Text (Text)

-- | Returns current node version string.
web3_clientVersion :: Provider a => Web3 a Text
{-# INLINE web3_clientVersion #-}
web3_clientVersion = remote "web3_clientVersion"

-- | Returns Keccak-256 (not the standardized SHA3-256) of the given data.
web3_sha3 :: Provider a => Text -> Web3 a Text
{-# INLINE web3_sha3 #-}
web3_sha3 = remote "web3_sha3"

-- | Returns the balance of the account of given address.
eth_getBalance :: Provider a => Address -> CallMode -> Web3 a Text
{-# INLINE eth_getBalance #-}
eth_getBalance = remote "eth_getBalance"

-- | Creates a filter object, based on filter options, to notify when the
-- state changes (logs). To check if the state has changed, call
-- 'getFilterChanges'.
eth_newFilter :: Provider a => Filter -> Web3 a FilterId
{-# INLINE eth_newFilter #-}
eth_newFilter = remote "eth_newFilter"

-- | Polling method for a filter, which returns an array of logs which
-- occurred since last poll.
eth_getFilterChanges :: Provider a => FilterId -> Web3 a [Change Text]
{-# INLINE eth_getFilterChanges #-}
eth_getFilterChanges = remote "eth_getFilterChanges"

-- | Uninstalls a filter with given id.
-- Should always be called when watch is no longer needed.
eth_uninstallFilter :: Provider a => FilterId -> Web3 a Bool
{-# INLINE eth_uninstallFilter #-}
eth_uninstallFilter = remote "eth_uninstallFilter"

-- | Executes a new message call immediately without creating a
-- transaction on the block chain.
eth_call :: Provider a => Call -> CallMode -> Web3 a Text
{-# INLINE eth_call #-}
eth_call = remote "eth_call"

-- | Creates new message call transaction or a contract creation,
-- if the data field contains code.
eth_sendTransaction :: Provider a => Call -> Web3 a Text
{-# INLINE eth_sendTransaction #-}
eth_sendTransaction = remote "eth_sendTransaction"

-- | Returns a list of addresses owned by client.
eth_accounts :: Provider a => Web3 a [Address]
{-# INLINE eth_accounts #-}
eth_accounts = remote "eth_accounts"

eth_newBlockFilter :: Provider a => Web3 a Text
{-# INLINE eth_newBlockFilter #-}
eth_newBlockFilter = remote "eth_newBlockFilter"

-- | Polling method for a block filter, which returns an array of block hashes
-- occurred since last poll.
eth_getBlockFilterChanges :: Provider a => Text -> Web3 a [Text]
{-# INLINE eth_getBlockFilterChanges #-}
eth_getBlockFilterChanges = remote "eth_getFilterChanges"

-- | Returns information about a block by hash.
eth_getBlockByHash :: Provider a => Text -> Web3 a Block
{-# INLINE eth_getBlockByHash #-}
eth_getBlockByHash = flip (remote "eth_getBlockByHash") True
