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

eth_newBlockFilter :: Web3 Text
eth_newBlockFilter = remote "eth_newBlockFilter"

-- | Polling method for a block filter, which returns an array of block hashes
-- occurred since last poll.
eth_getBlockFilterChanges :: Text -> Web3 [Text]
eth_getBlockFilterChanges = remote "eth_getFilterChanges"

data Transaction = Transaction
  { txHash              :: Text
  -- ^ DATA, 32 Bytes - hash of the transaction.
  , txNonce             :: Text
  -- ^ QUANTITY - the number of transactions made by the sender prior to this one.
  , txBlockHash         :: Text
  -- ^ DATA, 32 Bytes - hash of the block where this transaction was in. null when its pending.
  , txBlockNumber       :: Text
  -- ^ QUANTITY - block number where this transaction was in. null when its pending.
  , txTransactionIndex  :: Text
  -- ^ QUANTITY - integer of the transactions index position in the block. null when its pending.
  , txFrom              :: Text
  -- ^ DATA, 20 Bytes - address of the sender.
  , txTo                :: Maybe Text
  -- ^ DATA, 20 Bytes - address of the receiver. null when its a contract creation transaction.
  , txValue             :: Text
  -- ^ QUANTITY - value transferred in Wei.
  , txGasPrice          :: Text
  -- ^ QUANTITY - gas price provided by the sender in Wei.
  , txGas               :: Text
  -- ^ QUANTITY - gas provided by the sender.
  , txInput             :: Text
  -- ^ DATA - the data send along with the transaction.
  } deriving Show

$(deriveJSON (defaultOptions { fieldLabelModifier =
    let f (x : xs) = toLower x : xs in f . drop 2 }) ''Transaction)

data Block = Block
  { blockNumber         :: Text
  -- ^ QUANTITY - the block number. null when its pending block.
  , blockHash           :: Text
  -- ^ DATA, 32 Bytes - hash of the block. null when its pending block.
  , blockParentHash     :: Text
  -- ^ DATA, 32 Bytes - hash of the parent block.
  , blockNonce          :: Maybe Text
  -- ^ DATA, 8 Bytes - hash of the generated proof-of-work. null when its pending block.
  , blockSha3Uncles     :: Text
  -- ^ DATA, 32 Bytes - SHA3 of the uncles data in the block.
  , blockLogsBloom      :: Text
  -- ^ DATA, 256 Bytes - the bloom filter for the logs of the block. null when its pending block.
  , blockTransactionsRoot :: Text
  -- ^ DATA, 32 Bytes - the root of the transaction trie of the block.
  , blockStateRoot      :: Text
  -- ^ DATA, 32 Bytes - the root of the final state trie of the block.
  , blockReceiptRoot    :: Maybe Text
  -- ^ DATA, 32 Bytes - the root of the receipts trie of the block.
  , blockMiner          :: Text
  -- ^ DATA, 20 Bytes - the address of the beneficiary to whom the mining rewards were given.
  , blockDifficulty     :: Text
  -- ^ QUANTITY - integer of the difficulty for this block.
  , blockTotalDifficulty :: Text
  -- ^ QUANTITY - integer of the total difficulty of the chain until this block.
  , blockExtraData      :: Text
  -- ^ DATA - the "extra data" field of this block.
  , blockSize           :: Text
  -- ^ QUANTITY - integer the size of this block in bytes.
  , blockGasLimit       :: Text
  -- ^ QUANTITY - the maximum gas allowed in this block.
  , blockGasUsed        :: Text
  -- ^ QUANTITY - the total used gas by all transactions in this block.
  , blockTimestamp      :: Text
  -- ^ QUANTITY - the unix timestamp for when the block was collated.
  , blockTransactions   :: [Transaction]
  -- ^ Array of transaction objects.
  , blockUncles         :: [Text]
  -- ^ Array - Array of uncle hashes.
  } deriving Show

$(deriveJSON (defaultOptions { fieldLabelModifier =
    let f (x : xs) = toLower x : xs in f . drop 5 }) ''Block)

-- | Returns information about a block by hash.
eth_getBlockByHash :: Text -> Web3 Block
eth_getBlockByHash = flip (remote "eth_getBlockByHash") True
