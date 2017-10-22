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

-- | Returns the current network id.
net_version :: Provider a => Web3 a Text
{-# INLINE net_version #-}
net_version = remote "net_version"

-- | Returns true if client is actively listening for network connections.
net_listening :: Provider a => Web3 a Bool
{-# INLINE net_listening #-}
net_listening = remote "net_listening"

-- | Returns number of peers currently connected to the client.
net_peerCount :: Provider a => Web3 a Text
{-# INLINE net_peerCount #-}
net_peerCount = remote "net_peerCount"

-- | Returns the current ethereum protocol version.
eth_protocolVersion :: Provider a => Web3 a Text
{-# INLINE eth_protocolVersion #-}
eth_protocolVersion = remote "eth_protocolVersion"

-- | Returns the client coinbase address.
eth_coinbase :: Provider a => Web3 a Address
{-# INLINE eth_coinbase #-}
eth_coinbase = remote "eth_coinbase"

-- | Returns true if client is actively mining new blocks.
eth_mining :: Provider a => Web3 a Bool
{-# INLINE eth_mining #-}
eth_mining = remote "eth_mining"

-- | Returns the number of hashes per second that the node is mining with.
eth_hashrate :: Provider a => Web3 a Text
{-# INLINE eth_hashrate #-}
eth_hashrate = remote "eth_hashrate"

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
eth_getFilterChanges :: Provider a => FilterId -> Web3 a [Change]
{-# INLINE eth_getFilterChanges #-}
eth_getFilterChanges = remote "eth_getFilterChanges"

-- | Uninstalls a filter with given id.
-- Should always be called when watch is no longer needed.
eth_uninstallFilter :: Provider a => FilterId -> Web3 a Bool
{-# INLINE eth_uninstallFilter #-}
eth_uninstallFilter = remote "eth_uninstallFilter"

-- | Returns an array of all logs matching a given filter object.
eth_getLogs :: Provider a => Filter -> Web3 a [Change]
{-# INLINE eth_getLogs #-}
eth_getLogs = remote "eth_getLogs"

-- | Executes a new message call immediately without creating a
-- transaction on the block chain.
eth_call :: Provider a => Call -> CallMode -> Web3 a Text
{-# INLINE eth_call #-}
eth_call = remote "eth_call"

eth_sign :: Provider a => Address -> Text -> Web3 a Text
{-# INLINE eth_sign #-}
eth_sign = remote "eth_sign"

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

-- | Returns information about a block by block number.
eth_getBlockByNumber :: Provider a => Text -> Web3 a Block
{-# INLINE eth_getBlockByNumber #-}
eth_getBlockByNumber = flip (remote "eth_getBlockByNumber") True

-- | Returns the number of most recent block.
eth_blockNumber :: Provider a => Web3 a Text
{-# INLINE eth_blockNumber #-}
eth_blockNumber = remote "eth_blockNumber"

-- | Returns the number of transactions in a block matching the
-- given block number.
eth_getBlockTransactionCountByNumber :: Provider a => Text
                                     -> Web3 a Text
{-# INLINE eth_getBlockTransactionCountByNumber #-}
eth_getBlockTransactionCountByNumber =
    remote "eth_getBlockTransactionCountByNumber"

-- | Returns the current price per gas in wei.
eth_gasPrice :: Provider a => Web3 a Text
{-# INLINE eth_gasPrice #-}
eth_gasPrice = remote "eth_gasPrice"
