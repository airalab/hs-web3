-- |
-- Module      :  Network.Ethereum.Web3.Eth
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unknown
--
-- Ethereum node JSON-RPC API methods with `eth_` prefix.
--
module Network.Ethereum.Web3.Eth where

import Network.Ethereum.Web3.Provider
import Network.Ethereum.Web3.Address
import Network.Ethereum.Web3.JsonRpc
import Network.Ethereum.Web3.Types
import Data.Text (Text)

-- | Returns the current ethereum protocol version.
protocolVersion :: Provider a => Web3 a Text
{-# INLINE protocolVersion #-}
protocolVersion = remote "eth_protocolVersion"

-- TODO The return type of this function requires a new type to be created
-- | Returns an object with data about the sync status or false.
-- syncing :: Proviver a => Web3 a Text

-- | Returns the client coinbase address.
coinbase :: Provider a => Web3 a Address
{-# INLINE coinbase #-}
coinbase = remote "eth_coinbase"

-- | Returns true if client is actively mining new blocks.
mining :: Provider a => Web3 a Bool
{-# INLINE mining #-}
mining = remote "eth_mining"

-- | Returns the number of hashes per second that the node is mining with.
hashrate :: Provider a => Web3 a Text
{-# INLINE hashrate #-}
hashrate = remote "eth_hashrate"

-- | Returns the value from a storage position at a given address.
getStorageAt :: Provider a => Address -> Text -> DefaultBlock -> Web3 a Text
{-# INLINE getStorageAt #-}
getStorageAt = remote "eth_getStorageAt"

-- | Returns the number of transactions sent from an address.
getTransactionCount :: Provider a => Address -> DefaultBlock -> Web3 a Text
{-# INLINE getTransactionCount #-}
getTransactionCount = remote "eth_getTransactionCount"

-- | Returns the number of transactions in a block from a block matching the given block hash.
getBlockTransactionCountByHash :: Provider a => Text -> Web3 a Text
{-# INLINE getBlockTransactionCountByHash #-}
getBlockTransactionCountByHash = remote "eth_getBlockTransactionCountByHash"

-- | Returns the number of transactions in a block matching the
-- given block number.
getBlockTransactionCountByNumber :: Provider a => Text -> Web3 a Text
{-# INLINE getBlockTransactionCountByNumber #-}
getBlockTransactionCountByNumber = remote "eth_getBlockTransactionCountByNumber"

-- | Returns the number of uncles in a block from a block matching the given
-- block hash.
getUncleCountByBlockHash :: Provider a => Text -> Web3 a Text
{-# INLINE getUncleCountByBlockHash #-}
getUncleCountByBlockHash = remote "eth_getUncleCountByBlockHash"

-- | Returns the number of uncles in a block from a block matching the given
-- block number.
getUncleCountByBlockNumber :: Provider a => Text -> Web3 a Text
{-# INLINE getUncleCountByBlockNumber #-}
getUncleCountByBlockNumber = remote "eth_getUncleCountByBlockNumber"

-- | Returns code at a given address.
getCode :: Provider a => Address -> DefaultBlock -> Web3 a Text
{-# INLINE getCode #-}
getCode = remote "eth_getCode"

-- | Returns an Ethereum specific signature with:
-- sign(keccak256("\x19Ethereum Signed Message:\n" + len(message) + message))).
sign :: Provider a => Address -> Text -> Web3 a Text
{-# INLINE sign #-}
sign = remote "eth_sign"

-- | Creates new message call transaction or a contract creation,
-- if the data field contains code.
sendTransaction :: Provider a => Call -> Web3 a Text
{-# INLINE sendTransaction #-}
sendTransaction = remote "eth_sendTransaction"

-- | Creates new message call transaction or a contract creation for signed
-- transactions.
sendRawTransaction :: Provider a => Text -> Web3 a Text
{-# INLINE sendRawTransaction #-}
sendRawTransaction = remote "eth_sendRawTransaction"

-- | Returns the balance of the account of given address.
getBalance :: Provider a => Address -> DefaultBlock -> Web3 a Text
{-# INLINE getBalance #-}
getBalance = remote "eth_getBalance"

-- | Creates a filter object, based on filter options, to notify when the
-- state changes (logs). To check if the state has changed, call
-- 'getFilterChanges'.
newFilter :: Provider a => Filter -> Web3 a FilterId
{-# INLINE newFilter #-}
newFilter = remote "eth_newFilter"

-- | Polling method for a filter, which returns an array of logs which
-- occurred since last poll.
getFilterChanges :: Provider a => FilterId -> Web3 a [Change]
{-# INLINE getFilterChanges #-}
getFilterChanges = remote "eth_getFilterChanges"

-- | Uninstalls a filter with given id.
-- Should always be called when watch is no longer needed.
uninstallFilter :: Provider a => FilterId -> Web3 a Bool
{-# INLINE uninstallFilter #-}
uninstallFilter = remote "eth_uninstallFilter"

-- | Returns an array of all logs matching a given filter object.
getLogs :: Provider a => Filter -> Web3 a [Change]
{-# INLINE getLogs #-}
getLogs = remote "eth_getLogs"

-- | Executes a new message call immediately without creating a
-- transaction on the block chain.
call :: Provider a => Call -> DefaultBlock -> Web3 a Text
{-# INLINE call #-}
call = remote "eth_call"

-- | Makes a call or transaction, which won't be added to the blockchain and
-- returns the used gas, which can be used for estimating the used gas.
estimateGas :: Provider a => Call -> Web3 a Text
{-# INLINE estimateGas #-}
estimateGas = remote "eth_estimateGas"

-- | Returns information about a block by hash.
getBlockByHash :: Provider a => Text -> Web3 a Block
{-# INLINE getBlockByHash #-}
getBlockByHash = flip (remote "eth_getBlockByHash") True

-- | Returns information about a block by block number.
getBlockByNumber :: Provider a => Text -> Web3 a Block
{-# INLINE getBlockByNumber #-}
getBlockByNumber = flip (remote "eth_getBlockByNumber") True

-- | Returns the information about a transaction requested by transaction hash.
getTransactionByHash :: Provider a => Text -> Web3 a (Maybe Transaction)
{-# INLINE getTransactionByHash #-}
getTransactionByHash = remote "eth_getTransactionByHash"

-- | Returns information about a transaction by block hash and transaction index position.
getTransactionByBlockHashAndIndex :: Provider a => Text -> Text -> Web3 a (Maybe Transaction)
{-# INLINE getTransactionByBlockHashAndIndex #-}
getTransactionByBlockHashAndIndex = remote "eth_getTransactionByBlockHashAndIndex"

-- | Returns information about a transaction by block number and transaction
-- index position.
getTransactionByBlockNumberAndIndex :: Provider a => DefaultBlock -> Text -> Web3 a (Maybe Transaction)
{-# INLINE getTransactionByBlockNumberAndIndex #-}
getTransactionByBlockNumberAndIndex = remote "eth_getTransactionByBlockNumberAndIndex"

-- | Returns the receipt of a transaction by transaction hash.
-- TODO must create new type, TxReceipt
-- getTransactionReceipt :: Provider a => Text -> Web3 a TxReceipt
-- getTransactionReceipt = remote "getTransactionReceipt"

-- | Returns a list of addresses owned by client.
accounts :: Provider a => Web3 a [Address]
{-# INLINE accounts #-}
accounts = remote "eth_accounts"

newBlockFilter :: Provider a => Web3 a Text
{-# INLINE newBlockFilter #-}
newBlockFilter = remote "eth_newBlockFilter"

-- | Polling method for a block filter, which returns an array of block hashes
-- occurred since last poll.
getBlockFilterChanges :: Provider a => Text -> Web3 a [Text]
{-# INLINE getBlockFilterChanges #-}
getBlockFilterChanges = remote "eth_getBlockFilterChanges"

-- | Returns the number of most recent block.
blockNumber :: Provider a => Web3 a Text
{-# INLINE blockNumber #-}
blockNumber = remote "eth_blockNumber"

-- | Returns the current price per gas in wei.
gasPrice :: Provider a => Web3 a Text
{-# INLINE gasPrice #-}
gasPrice = remote "eth_gasPrice"

-- | Returns information about a uncle of a block by hash and uncle index
-- position.
getUncleByBlockHashAndIndex :: Provider a => Text -> Text -> Web3 a Block
{-# INLINE getUncleByBlockHashAndIndex #-}
getUncleByBlockHashAndIndex = remote "eth_getUncleByBlockHashAndIndex"

-- | Returns information about a uncle of a block by number and uncle index
-- position.
getUncleByBlockNumberAndIndex :: Provider a => DefaultBlock -> Text -> Web3 a Block
{-# INLINE getUncleByBlockNumberAndIndex #-}
getUncleByBlockNumberAndIndex = remote "eth_getUncleByBlockNumberAndIndex"

-- | Creates a filter in the node, to notify when new pending transactions arrive. To check if the state has changed, call getFilterChanges. Returns a FilterId.
newPendingTransactionFilter :: Provider a => Web3 a Text
{-# INLINE newPendingTransactionFilter #-}
newPendingTransactionFilter = remote "eth_newPendingTransactionFilter"

-- | Returns an array of all logs matching filter with given id.
getFilterLogs :: Provider a => Text -> Web3 a [Change]
{-# INLINE getFilterLogs #-}
getFilterLogs = remote "eth_getFilterLogs"

-- | Returns the hash of the current block, the seedHash, and the boundary
-- condition to be met ("target").
getWork :: Provider a => Web3 a [Text]
{-# INLINE getWork #-}
getWork = remote "eth_getWork"

-- | Used for submitting a proof-of-work solution.
-- Parameters:
-- 1. DATA, 8 Bytes - The nonce found (64 bits)
-- 2. DATA, 32 Bytes - The header's pow-hash (256 bits)
-- 3. DATA, 32 Bytes - The mix digest (256 bits)
submitWork :: Provider a => Text -> Text -> Text -> Web3 a Bool
{-# INLINE submitWork #-}
submitWork = remote "eth_submitWork"

-- | Used for submitting mining hashrate.
-- Parameters:
-- 1. Hashrate, a hexadecimal string representation (32 bytes) of the hash rate
-- 2. ID, String - A random hexadecimal(32 bytes) ID identifying the client
submitHashrate :: Provider a => Text -> Text -> Web3 a Bool
{-# INLINE submitHashrate #-}
submitHashrate = remote "eth_submitHashrate"
