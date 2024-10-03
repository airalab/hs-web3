{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Ethereum.Web3.Eth
-- Copyright   :  Aleksandr Krupenkin 2016-2024
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unknown
--
-- Ethereum node JSON-RPC API methods with `eth_` prefix.
--

module Network.Ethereum.Api.Eth where

import           Data.ByteArray.HexString   (HexString)
import           Data.Solidity.Prim.Address (Address)
import           Data.Text                  (Text)
import           Network.Ethereum.Api.Types (Block, BlockT, Call, Change,
                                             DefaultBlock, Filter, Quantity,
                                             SyncingState, Transaction,
                                             TxReceipt)
import           Network.JsonRpc.TinyClient (JsonRpc (..))

-- | Returns the current ethereum protocol version.
protocolVersion :: JsonRpc m => m Text
{-# INLINE protocolVersion #-}
protocolVersion = remote "eth_protocolVersion"

-- | Returns an object with data about the sync status or false.
syncing :: JsonRpc m => m SyncingState
{-# INLINE syncing #-}
syncing = remote "eth_syncing"

-- | Returns the client coinbase address.
coinbase :: JsonRpc m => m Address
{-# INLINE coinbase #-}
coinbase = remote "eth_coinbase"

-- | Returns true if client is actively mining new blocks.
mining :: JsonRpc m => m Bool
{-# INLINE mining #-}
mining = remote "eth_mining"

-- | Returns the number of hashes per second that the node is mining with.
hashrate :: JsonRpc m => m Quantity
{-# INLINE hashrate #-}
hashrate = remote "eth_hashrate"

-- | Returns the value from a storage position at a given address.
getStorageAt :: JsonRpc m => Address -> Quantity -> DefaultBlock -> m HexString
{-# INLINE getStorageAt #-}
getStorageAt = remote "eth_getStorageAt"

-- | Returns the number of transactions sent from an address.
getTransactionCount :: JsonRpc m => Address -> DefaultBlock -> m Quantity
{-# INLINE getTransactionCount #-}
getTransactionCount = remote "eth_getTransactionCount"

-- | Returns the number of transactions in a block from a block matching the given block hash.
getBlockTransactionCountByHash :: JsonRpc m => HexString -> m Quantity
{-# INLINE getBlockTransactionCountByHash #-}
getBlockTransactionCountByHash = remote "eth_getBlockTransactionCountByHash"

-- | Returns the number of transactions in a block matching the
-- given block number.
getBlockTransactionCountByNumber :: JsonRpc m => Quantity -> m Quantity
{-# INLINE getBlockTransactionCountByNumber #-}
getBlockTransactionCountByNumber = remote "eth_getBlockTransactionCountByNumber"

-- | Returns the number of uncles in a block from a block matching the given
-- block hash.
getUncleCountByBlockHash :: JsonRpc m => HexString -> m Quantity
{-# INLINE getUncleCountByBlockHash #-}
getUncleCountByBlockHash = remote "eth_getUncleCountByBlockHash"

-- | Returns the number of uncles in a block from a block matching the given
-- block number.
getUncleCountByBlockNumber :: JsonRpc m => Quantity -> m Quantity
{-# INLINE getUncleCountByBlockNumber #-}
getUncleCountByBlockNumber = remote "eth_getUncleCountByBlockNumber"

-- | Returns code at a given address.
getCode :: JsonRpc m => Address -> DefaultBlock -> m HexString
{-# INLINE getCode #-}
getCode = remote "eth_getCode"

-- | Returns an Ethereum specific signature with:
-- sign(keccak256("\x19Ethereum Signed Message:\n" + len(message) + message))).
sign :: JsonRpc m => Address -> HexString -> m HexString
{-# INLINE sign #-}
sign = remote "eth_sign"

-- | Creates new message call transaction or a contract creation,
-- if the data field contains code.
sendTransaction :: JsonRpc m => Call -> m HexString
{-# INLINE sendTransaction #-}
sendTransaction = remote "eth_sendTransaction"

-- | Creates new message call transaction or a contract creation for signed
-- transactions.
sendRawTransaction :: JsonRpc m => HexString -> m HexString
{-# INLINE sendRawTransaction #-}
sendRawTransaction = remote "eth_sendRawTransaction"

-- | Returns the balance of the account of given address.
getBalance :: JsonRpc m => Address -> DefaultBlock -> m Quantity
{-# INLINE getBalance #-}
getBalance = remote "eth_getBalance"

-- | Creates a filter object, based on filter options, to notify when the
-- state changes (logs). To check if the state has changed, call
-- 'getFilterChanges'.
newFilter :: JsonRpc m => Filter e -> m Quantity
{-# INLINE newFilter #-}
newFilter = remote "eth_newFilter"

-- | Polling method for a filter, which returns an array of logs which
-- occurred since last poll.
getFilterChanges :: JsonRpc m => Quantity -> m [Change]
{-# INLINE getFilterChanges #-}
getFilterChanges = remote "eth_getFilterChanges"

-- | Uninstalls a filter with given id.
-- Should always be called when watch is no longer needed.
uninstallFilter :: JsonRpc m => Quantity -> m Bool
{-# INLINE uninstallFilter #-}
uninstallFilter = remote "eth_uninstallFilter"

-- | Returns an array of all logs matching a given filter object.
getLogs :: JsonRpc m => Filter e -> m [Change]
{-# INLINE getLogs #-}
getLogs = remote "eth_getLogs"

-- | Executes a new message call immediately without creating a
-- transaction on the block chain.
call :: JsonRpc m => Call -> DefaultBlock -> m HexString
{-# INLINE call #-}
call = remote "eth_call"

-- | Makes a call or transaction, which won't be added to the blockchain and
-- returns the used gas, which can be used for estimating the used gas.
estimateGas :: JsonRpc m => Call -> m Quantity
{-# INLINE estimateGas #-}
estimateGas = remote "eth_estimateGas"

-- | Returns information about a block by hash with only hashes of the transactions in it.
getBlockByHashLite :: JsonRpc m => HexString -> m (Maybe (BlockT HexString))
{-# INLINE getBlockByHashLite #-}
getBlockByHashLite = flip (remote "eth_getBlockByHash") False

-- | Returns information about a block by block number with only hashes of the transactions in it.
getBlockByNumberLite :: JsonRpc m => Quantity -> m (Maybe (BlockT HexString))
{-# INLINE getBlockByNumberLite #-}
getBlockByNumberLite = flip (remote "eth_getBlockByNumber") False

-- | Returns information about a block by hash.
getBlockByHash :: JsonRpc m => HexString -> m (Maybe Block)
{-# INLINE getBlockByHash #-}
getBlockByHash = flip (remote "eth_getBlockByHash") True

-- | Returns information about a block by block number.
getBlockByNumber :: JsonRpc m => Quantity -> m (Maybe Block)
{-# INLINE getBlockByNumber #-}
getBlockByNumber = flip (remote "eth_getBlockByNumber") True

-- | Returns the information about a transaction requested by transaction hash.
getTransactionByHash :: JsonRpc m => HexString -> m (Maybe Transaction)
{-# INLINE getTransactionByHash #-}
getTransactionByHash = remote "eth_getTransactionByHash"

-- | Returns information about a transaction by block hash and transaction index position.
getTransactionByBlockHashAndIndex :: JsonRpc m => HexString -> Quantity -> m (Maybe Transaction)
{-# INLINE getTransactionByBlockHashAndIndex #-}
getTransactionByBlockHashAndIndex = remote "eth_getTransactionByBlockHashAndIndex"

-- | Returns information about a transaction by block number and transaction
-- index position.
getTransactionByBlockNumberAndIndex :: JsonRpc m => DefaultBlock -> Quantity -> m (Maybe Transaction)
{-# INLINE getTransactionByBlockNumberAndIndex #-}
getTransactionByBlockNumberAndIndex = remote "eth_getTransactionByBlockNumberAndIndex"

-- | Returns the receipt of a transaction by transaction hash.
getTransactionReceipt :: JsonRpc m => HexString -> m (Maybe TxReceipt)
{-# INLINE getTransactionReceipt #-}
getTransactionReceipt = remote "eth_getTransactionReceipt"

-- | Returns a list of addresses owned by client.
accounts :: JsonRpc m => m [Address]
{-# INLINE accounts #-}
accounts = remote "eth_accounts"

-- | Creates a filter in the node, to notify when a new block arrives.
newBlockFilter :: JsonRpc m => m Quantity
{-# INLINE newBlockFilter #-}
newBlockFilter = remote "eth_newBlockFilter"

-- | Polling method for a block filter, which returns an array of block hashes
-- occurred since last poll.
getBlockFilterChanges :: JsonRpc m => Quantity -> m [HexString]
{-# INLINE getBlockFilterChanges #-}
getBlockFilterChanges = remote "eth_getFilterChanges"

-- | Returns the number of most recent block.
blockNumber :: JsonRpc m => m Quantity
{-# INLINE blockNumber #-}
blockNumber = remote "eth_blockNumber"

-- | Returns the current price per gas in wei.
gasPrice :: JsonRpc m => m Quantity
{-# INLINE gasPrice #-}
gasPrice = remote "eth_gasPrice"

-- | Returns information about a uncle of a block by hash and uncle index
-- position.
getUncleByBlockHashAndIndex :: JsonRpc m => HexString -> Quantity -> m Block
{-# INLINE getUncleByBlockHashAndIndex #-}
getUncleByBlockHashAndIndex = remote "eth_getUncleByBlockHashAndIndex"

-- | Returns information about a uncle of a block by number and uncle index
-- position.
getUncleByBlockNumberAndIndex :: JsonRpc m => DefaultBlock -> Quantity -> m Block
{-# INLINE getUncleByBlockNumberAndIndex #-}
getUncleByBlockNumberAndIndex = remote "eth_getUncleByBlockNumberAndIndex"

-- | Creates a filter in the node, to notify when new pending transactions arrive. To check if the state has changed, call getFilterChanges. Returns a FilterId.
newPendingTransactionFilter :: JsonRpc m => m Quantity
{-# INLINE newPendingTransactionFilter #-}
newPendingTransactionFilter = remote "eth_newPendingTransactionFilter"

-- | Returns an array of all logs matching filter with given id.
getFilterLogs :: JsonRpc m => Quantity -> m [Change]
{-# INLINE getFilterLogs #-}
getFilterLogs = remote "eth_getFilterLogs"

-- | Returns the hash of the current block, the seedHash, and the boundary
-- condition to be met ("target").
getWork :: JsonRpc m => m [HexString]
{-# INLINE getWork #-}
getWork = remote "eth_getWork"

-- | Used for submitting a proof-of-work solution.
-- Parameters:
-- 1. DATA, 8 Bytes - The nonce found (64 bits)
-- 2. DATA, 32 Bytes - The header's pow-hash (256 bits)
-- 3. DATA, 32 Bytes - The mix digest (256 bits)
submitWork :: JsonRpc m => HexString -> HexString -> HexString -> m Bool
{-# INLINE submitWork #-}
submitWork = remote "eth_submitWork"

-- | Used for submitting mining hashrate.
-- Parameters:
-- 1. Hashrate, a hexadecimal string representation (32 bytes) of the hash rate
-- 2. ID, String - A random hexadecimal(32 bytes) ID identifying the client
submitHashrate :: JsonRpc m => HexString -> HexString -> m Bool
{-# INLINE submitHashrate #-}
submitHashrate = remote "eth_submitHashrate"
