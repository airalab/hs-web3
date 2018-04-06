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

import           Data.Text                      (Text)
import           Network.Ethereum.Web3.Address
import           Network.Ethereum.Web3.JsonRpc
import           Network.Ethereum.Web3.Provider
import           Network.Ethereum.Web3.Types

-- | Returns the current ethereum protocol version.
protocolVersion :: Web3 Text
{-# INLINE protocolVersion #-}
protocolVersion = remote "eth_protocolVersion"

-- | Returns an object with data about the sync status or false.
syncing :: Web3 SyncingState
{-# INLINE syncing #-}
syncing = remote "eth_syncing"

-- | Returns the client coinbase address.
coinbase :: Web3 Address
{-# INLINE coinbase #-}
coinbase = remote "eth_coinbase"

-- | Returns true if client is actively mining new blocks.
mining :: Web3 Bool
{-# INLINE mining #-}
mining = remote "eth_mining"

-- | Returns the number of hashes per second that the node is mining with.
hashrate :: Web3 Quantity
{-# INLINE hashrate #-}
hashrate = remote "eth_hashrate"

-- | Returns the value from a storage position at a given address.
getStorageAt :: Address -> Text -> DefaultBlock -> Web3 Text
{-# INLINE getStorageAt #-}
getStorageAt = remote "eth_getStorageAt"

-- | Returns the number of transactions sent from an address.
getTransactionCount :: Address -> DefaultBlock -> Web3 Quantity
{-# INLINE getTransactionCount #-}
getTransactionCount = remote "eth_getTransactionCount"

-- | Returns the number of transactions in a block from a block matching the given block hash.
getBlockTransactionCountByHash :: Text -> Web3 Quantity
{-# INLINE getBlockTransactionCountByHash #-}
getBlockTransactionCountByHash = remote "eth_getBlockTransactionCountByHash"

-- | Returns the number of transactions in a block matching the
-- given block number.
getBlockTransactionCountByNumber :: Text -> Web3 Quantity
{-# INLINE getBlockTransactionCountByNumber #-}
getBlockTransactionCountByNumber = remote "eth_getBlockTransactionCountByNumber"

-- | Returns the number of uncles in a block from a block matching the given
-- block hash.
getUncleCountByBlockHash :: Text -> Web3 Quantity
{-# INLINE getUncleCountByBlockHash #-}
getUncleCountByBlockHash = remote "eth_getUncleCountByBlockHash"

-- | Returns the number of uncles in a block from a block matching the given
-- block number.
getUncleCountByBlockNumber :: Text -> Web3 Quantity
{-# INLINE getUncleCountByBlockNumber #-}
getUncleCountByBlockNumber = remote "eth_getUncleCountByBlockNumber"

-- | Returns code at a given address.
getCode :: Address -> DefaultBlock -> Web3 Text
{-# INLINE getCode #-}
getCode = remote "eth_getCode"

-- | Returns an Ethereum specific signature with:
-- sign(keccak256("\x19Ethereum Signed Message:\n" + len(message) + message))).
sign :: Address -> Text -> Web3 Text
{-# INLINE sign #-}
sign = remote "eth_sign"

-- | Creates new message call transaction or a contract creation,
-- if the data field contains code.
sendTransaction :: Call -> Web3 Text
{-# INLINE sendTransaction #-}
sendTransaction = remote "eth_sendTransaction"

-- | Creates new message call transaction or a contract creation for signed
-- transactions.
sendRawTransaction :: Text -> Web3 Text
{-# INLINE sendRawTransaction #-}
sendRawTransaction = remote "eth_sendRawTransaction"

-- | Returns the balance of the account of given address.
getBalance :: Address -> DefaultBlock -> Web3 Quantity
{-# INLINE getBalance #-}
getBalance = remote "eth_getBalance"

-- | Creates a filter object, based on filter options, to notify when the
-- state changes (logs). To check if the state has changed, call
-- 'getFilterChanges'.
newFilter :: Filter e -> Web3 FilterId
{-# INLINE newFilter #-}
newFilter = remote "eth_newFilter"

-- | Polling method for a filter, which returns an array of logs which
-- occurred since last poll.
getFilterChanges :: FilterId -> Web3 [Change]
{-# INLINE getFilterChanges #-}
getFilterChanges = remote "eth_getFilterChanges"

-- | Uninstalls a filter with given id.
-- Should always be called when watch is no longer needed.
uninstallFilter :: FilterId -> Web3 Bool
{-# INLINE uninstallFilter #-}
uninstallFilter = remote "eth_uninstallFilter"

-- | Returns an array of all logs matching a given filter object.
getLogs :: Filter e -> Web3 [Change]
{-# INLINE getLogs #-}
getLogs = remote "eth_getLogs"

-- | Executes a new message call immediately without creating a
-- transaction on the block chain.
call :: Call -> DefaultBlock -> Web3 Text
{-# INLINE call #-}
call = remote "eth_call"

-- | Makes a call or transaction, which won't be added to the blockchain and
-- returns the used gas, which can be used for estimating the used gas.
estimateGas :: Call -> Web3 Quantity
{-# INLINE estimateGas #-}
estimateGas = remote "eth_estimateGas"

-- | Returns information about a block by hash.
getBlockByHash :: Text -> Web3 Block
{-# INLINE getBlockByHash #-}
getBlockByHash = flip (remote "eth_getBlockByHash") True

-- | Returns information about a block by block number.
getBlockByNumber :: Text -> Web3 Block
{-# INLINE getBlockByNumber #-}
getBlockByNumber = flip (remote "eth_getBlockByNumber") True

-- | Returns the information about a transaction requested by transaction hash.
getTransactionByHash :: Text -> Web3 (Maybe Transaction)
{-# INLINE getTransactionByHash #-}
getTransactionByHash = remote "eth_getTransactionByHash"

-- | Returns information about a transaction by block hash and transaction index position.
getTransactionByBlockHashAndIndex :: Text -> Text -> Web3 (Maybe Transaction)
{-# INLINE getTransactionByBlockHashAndIndex #-}
getTransactionByBlockHashAndIndex = remote "eth_getTransactionByBlockHashAndIndex"

-- | Returns information about a transaction by block number and transaction
-- index position.
getTransactionByBlockNumberAndIndex :: DefaultBlock -> Text -> Web3 (Maybe Transaction)
{-# INLINE getTransactionByBlockNumberAndIndex #-}
getTransactionByBlockNumberAndIndex = remote "eth_getTransactionByBlockNumberAndIndex"

-- | Returns the receipt of a transaction by transaction hash.
getTransactionReceipt :: TxHash -> Web3 (Maybe TxReceipt)
{-# INLINE getTransactionReceipt #-}
getTransactionReceipt = remote "eth_getTransactionReceipt"

-- | Returns a list of addresses owned by client.
accounts :: Web3 [Address]
{-# INLINE accounts #-}
accounts = remote "eth_accounts"

newBlockFilter :: Web3 Text
{-# INLINE newBlockFilter #-}
newBlockFilter = remote "eth_newBlockFilter"

-- | Polling method for a block filter, which returns an array of block hashes
-- occurred since last poll.
getBlockFilterChanges :: Text -> Web3 [Text]
{-# INLINE getBlockFilterChanges #-}
getBlockFilterChanges = remote "eth_getBlockFilterChanges"

-- | Returns the number of most recent block.
blockNumber :: Web3 BlockNumber
{-# INLINE blockNumber #-}
blockNumber = remote "eth_blockNumber"

-- | Returns the current price per gas in wei.
gasPrice :: Web3 Quantity
{-# INLINE gasPrice #-}
gasPrice = remote "eth_gasPrice"

-- | Returns information about a uncle of a block by hash and uncle index
-- position.
getUncleByBlockHashAndIndex :: Text -> Text -> Web3 Block
{-# INLINE getUncleByBlockHashAndIndex #-}
getUncleByBlockHashAndIndex = remote "eth_getUncleByBlockHashAndIndex"

-- | Returns information about a uncle of a block by number and uncle index
-- position.
getUncleByBlockNumberAndIndex :: DefaultBlock -> Text -> Web3 Block
{-# INLINE getUncleByBlockNumberAndIndex #-}
getUncleByBlockNumberAndIndex = remote "eth_getUncleByBlockNumberAndIndex"

-- | Creates a filter in the node, to notify when new pending transactions arrive. To check if the state has changed, call getFilterChanges. Returns a FilterId.
newPendingTransactionFilter :: Web3 Text
{-# INLINE newPendingTransactionFilter #-}
newPendingTransactionFilter = remote "eth_newPendingTransactionFilter"

-- | Returns an array of all logs matching filter with given id.
getFilterLogs :: Text -> Web3 [Change]
{-# INLINE getFilterLogs #-}
getFilterLogs = remote "eth_getFilterLogs"

-- | Returns the hash of the current block, the seedHash, and the boundary
-- condition to be met ("target").
getWork :: Web3 [Text]
{-# INLINE getWork #-}
getWork = remote "eth_getWork"

-- | Used for submitting a proof-of-work solution.
-- Parameters:
-- 1. DATA, 8 Bytes - The nonce found (64 bits)
-- 2. DATA, 32 Bytes - The header's pow-hash (256 bits)
-- 3. DATA, 32 Bytes - The mix digest (256 bits)
submitWork :: Text -> Text -> Text -> Web3 Bool
{-# INLINE submitWork #-}
submitWork = remote "eth_submitWork"

-- | Used for submitting mining hashrate.
-- Parameters:
-- 1. Hashrate, a hexadecimal string representation (32 bytes) of the hash rate
-- 2. ID, String - A random hexadecimal(32 bytes) ID identifying the client
submitHashrate :: Text -> Text -> Web3 Bool
{-# INLINE submitHashrate #-}
submitHashrate = remote "eth_submitHashrate"
