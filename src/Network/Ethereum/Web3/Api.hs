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
web3_clientVersion = remote "web3_clientVersion"

-- | Returns Keccak-256 (not the standardized SHA3-256) of the given data.
web3_sha3 :: Provider a => Text -> Web3 a Text
web3_sha3 = remote "web3_sha3"

-- | Returns the current network id.
net_version :: Provider a => Web3 a Text
net_version = remote "net_version"

-- | Returns true if client is actively listening for network connections.
net_listening :: Provider a => Web3 a Bool
net_listening = remote "net_listening"

-- | Returns number of peers currently connected to the client.
net_peerCount :: Provider a => Web3 a Text
net_peerCount = remote "net_peerCount"

-- | Returns the current ethereum protocol version.
eth_protocolVersion :: Provider a => Web3 a Text
eth_protocolVersion = remote "eth_protocolVersion"

-- | Returns the client coinbase address.
eth_coinbase :: Provider a => Web3 a Address
eth_coinbase = remote "eth_coinbase"

-- | Returns true if client is actively mining new blocks.
eth_mining :: Provider a => Web3 a Bool
eth_mining = remote "eth_mining"

-- | Returns the number of hashes per second that the node is mining with.
eth_hashrate :: Provider a => Web3 a Text
eth_hashrate = remote "eth_hashrate"

-- | Returns the value from a storage position at a given address.
eth_getStorageAt :: Provider a => Address -> Text -> CallMode -> Web3 a Text
eth_getStorageAt = remote "eth_getStorageAt"

-- | Returns the number of transactions sent from an address.
eth_getTransactionCount :: Provider a => Address -> CallMode -> Web3 a Text
eth_getTransactionCount = remote "eth_getTransactionCount"

-- | Returns the number of transactions in a block from a block matching the given block hash.
eth_getBlockTransactionCountByHash :: Provider a => Web3 a Text
eth_getBlockTransactionCountByHash = remote "eth_getBlockTransactionCountByHash"

-- | Returns the number of transactions in a block matching the
-- given block number.
eth_getBlockTransactionCountByNumber :: Provider a => Text -> Web3 a Text
eth_getBlockTransactionCountByNumber =
    remote "eth_getBlockTransactionCountByNumber"

-- | Returns the number of uncles in a block from a block matching the given
-- block hash.
eth_getUncleCountByBlockHash :: Provider a => Text -> Web3 a Text
eth_getUncleCountByBlockHash = remote "eth_getUncleCountByBlockHash"

eth_getUncleCountByBlockNumber :: Provider a => Text -> Web3 a Text
eth_getUncleCountByBlockNumber = remote "eth_getUncleCountByBlockNumber"

-- | Returns the balance of the account of given address.
eth_getBalance :: Provider a => Address -> CallMode -> Web3 a Text
eth_getBalance = remote "eth_getBalance"

-- | Creates a filter object, based on filter options, to notify when the
-- state changes (logs). To check if the state has changed, call
-- 'getFilterChanges'.
eth_newFilter :: Provider a => Filter -> Web3 a FilterId
eth_newFilter = remote "eth_newFilter"

-- | Polling method for a filter, which returns an array of logs which
-- occurred since last poll.
eth_getFilterChanges :: Provider a => FilterId -> Web3 a [Change]
eth_getFilterChanges = remote "eth_getFilterChanges"

-- | Uninstalls a filter with given id.
-- Should always be called when watch is no longer needed.
eth_uninstallFilter :: Provider a => FilterId -> Web3 a Bool
eth_uninstallFilter = remote "eth_uninstallFilter"

-- | Returns an array of all logs matching a given filter object.
eth_getLogs :: Provider a => Filter -> Web3 a [Change]
eth_getLogs = remote "eth_getLogs"

-- | Executes a new message call immediately without creating a
-- transaction on the block chain.
eth_call :: Provider a => Call -> CallMode -> Web3 a Text
eth_call = remote "eth_call"

-- | Returns an Ethereum specific signature with:
-- sign(keccak256("\x19Ethereum Signed Message:\n" + len(message) + message))).
eth_sign :: Provider a => Address -> Text -> Web3 a Text
eth_sign = remote "eth_sign"

-- | Returns code at a given address.
eth_getCode :: Provider a => Address -> CallMode -> Web3 a Text
eth_getCode = remote "eth_getCode"

-- | Creates new message call transaction or a contract creation,
-- if the data field contains code.
eth_sendTransaction :: Provider a => Call -> Web3 a Text
eth_sendTransaction = remote "eth_sendTransaction"

-- | Returns a list of addresses owned by client.
eth_accounts :: Provider a => Web3 a [Address]
eth_accounts = remote "eth_accounts"

eth_newBlockFilter :: Provider a => Web3 a Text
eth_newBlockFilter = remote "eth_newBlockFilter"

-- | Polling method for a block filter, which returns an array of block hashes
-- occurred since last poll.
eth_getBlockFilterChanges :: Provider a => Text -> Web3 a [Text]
eth_getBlockFilterChanges = remote "eth_getFilterChanges"

-- | Returns information about a block by hash.
eth_getBlockByHash :: Provider a => Text -> Web3 a Block
eth_getBlockByHash = flip (remote "eth_getBlockByHash") True

-- | Returns information about a block by block number.
eth_getBlockByNumber :: Provider a => Text -> Web3 a Block
eth_getBlockByNumber = flip (remote "eth_getBlockByNumber") True

-- | Returns the number of most recent block.
eth_blockNumber :: Provider a => Web3 a Text
eth_blockNumber = remote "eth_blockNumber"

-- | Returns the current price per gas in wei.
eth_gasPrice :: Provider a => Web3 a Text
eth_gasPrice = remote "eth_gasPrice"

{-# INLINE web3_clientVersion #-}
{-# INLINE web3_sha3 #-}
{-# INLINE net_version #-}
{-# INLINE net_listening #-}
{-# INLINE net_peerCount #-}
{-# INLINE eth_protocolVersion #-}
{-# INLINE eth_coinbase #-}
{-# INLINE eth_mining #-}
{-# INLINE eth_hashrate #-}
{-# INLINE eth_getStorageAt #-}
{-# INLINE eth_getTransactionCount #-}
{-# INLINE eth_getBlockTransactionCountByHash #-}
{-# INLINE eth_getUncleCountByBlockHash #-}
{-# INLINE eth_getUncleCountByBlockNumber #-}
{-# INLINE eth_getBalance #-}
{-# INLINE eth_newFilter #-}
{-# INLINE eth_getFilterChanges #-}
{-# INLINE eth_uninstallFilter #-}
{-# INLINE eth_getLogs #-}
{-# INLINE eth_call #-}
{-# INLINE eth_sign #-}
{-# INLINE eth_getCode #-}
{-# INLINE eth_sendTransaction #-}
{-# INLINE eth_accounts #-}
{-# INLINE eth_newBlockFilter #-}
{-# INLINE eth_getBlockFilterChanges #-}
{-# INLINE eth_getBlockByHash #-}
{-# INLINE eth_getBlockByNumber #-}
{-# INLINE eth_blockNumber #-}
{-# INLINE eth_getBlockTransactionCountByNumber #-}
{-# INLINE eth_gasPrice #-}
