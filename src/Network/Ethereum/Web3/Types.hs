{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE TemplateHaskell            #-}
-- |
-- Module      :  Network.Ethereum.Web3.Types
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Common used types and instances.
--
module Network.Ethereum.Web3.Types where

import Network.Ethereum.Web3.Internal (toLowerFirst)
import qualified Data.Text.Lazy.Builder.Int as B
import qualified Data.Text.Lazy.Builder     as B
import qualified Data.Text.Read             as R
import Network.Ethereum.Web3.Address (Address)
import Control.Monad.IO.Class (MonadIO)
import Control.Exception (Exception)
import Data.Typeable (Typeable)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Aeson.TH
import Data.Aeson

-- | Any communication with Ethereum node wrapped with 'Web3' monad
newtype Web3 a b = Web3 { unWeb3 :: IO b }
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Some peace of error response
data Web3Error
  = JsonRpcFail RpcError
  -- ^ JSON-RPC communication error
  | ParserFail  String
  -- ^ Error in parser state
  | UserFail    String
  -- ^ Common head for user errors
  deriving (Typeable, Show, Eq)

instance Exception Web3Error

-- | JSON-RPC error message
data RpcError = RpcError
  { errCode     :: Int
  , errMessage  :: Text
  , errData     :: Maybe Value
  } deriving (Show, Eq)

$(deriveJSON (defaultOptions
    { fieldLabelModifier = toLowerFirst . drop 3 }) ''RpcError)

-- | Low-level event filter data structure
data Filter = Filter
  { filterAddress   :: Maybe Address
  , filterTopics    :: Maybe [Maybe Text]
  , filterFromBlock :: Maybe Text
  , filterToBlock   :: Maybe Text
  } deriving Show

$(deriveJSON (defaultOptions
    { fieldLabelModifier = toLowerFirst . drop 6 }) ''Filter)

-- | Event filder ident
newtype FilterId = FilterId Int
  deriving (Show, Eq, Ord)

instance FromJSON FilterId where
    parseJSON (String v) =
        case R.hexadecimal v of
            Right (x, "") -> return (FilterId x)
            _ -> fail "Unable to parse FilterId!"
    parseJSON _ = fail "The string is required!"

instance ToJSON FilterId where
    toJSON (FilterId x) =
        let hexValue = B.toLazyText (B.hexadecimal x)
        in  toJSON ("0x" <> hexValue)

-- | Changes pulled by low-level call 'eth_getFilterChanges'
data Change = Change
  { changeLogIndex         :: Text
  , changeTransactionIndex :: Text
  , changeTransactionHash  :: Text
  , changeBlockHash        :: Text
  , changeBlockNumber      :: Text
  , changeAddress          :: Address
  , changeData             :: Text
  , changeTopics           :: [Text]
  } deriving Show

$(deriveJSON (defaultOptions
    { fieldLabelModifier = toLowerFirst . drop 6 }) ''Change)

-- | The contract call params
data Call = Call
  { callFrom    :: Maybe Address
  , callTo      :: Address
  , callGas     :: Maybe Text
  , callPrice   :: Maybe Text
  , callValue   :: Maybe Text
  , callData    :: Maybe Text
  } deriving Show

$(deriveJSON (defaultOptions
    { fieldLabelModifier = toLowerFirst . drop 4 }) ''Call)

-- | The contract call mode describe used state: latest or pending
data CallMode = Latest | Pending
  deriving (Show, Eq)

instance ToJSON CallMode where
    toJSON = toJSON . toLowerFirst . show

-- TODO: Wrap
-- | Transaction hash text string
type TxHash = Text

-- | Transaction information
data Transaction = Transaction
  { txHash              :: TxHash
  -- ^ DATA, 32 Bytes - hash of the transaction.
  , txNonce             :: Text
  -- ^ QUANTITY - the number of transactions made by the sender prior to this one.
  , txBlockHash         :: Text
  -- ^ DATA, 32 Bytes - hash of the block where this transaction was in. null when its pending.
  , txBlockNumber       :: Text
  -- ^ QUANTITY - block number where this transaction was in. null when its pending.
  , txTransactionIndex  :: Text
  -- ^ QUANTITY - integer of the transactions index position in the block. null when its pending.
  , txFrom              :: Address
  -- ^ DATA, 20 Bytes - address of the sender.
  , txTo                :: Maybe Address
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

$(deriveJSON (defaultOptions
    { fieldLabelModifier = toLowerFirst . drop 2 }) ''Transaction)

-- | Block information
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
  , blockMiner          :: Address
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

$(deriveJSON (defaultOptions
    { fieldLabelModifier = toLowerFirst . drop 5 }) ''Block)
