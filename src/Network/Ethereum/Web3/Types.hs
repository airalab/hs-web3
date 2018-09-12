{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
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
-- Ethereum generic JSON-RPC types.
--

module Network.Ethereum.Web3.Types where

import           Data.Aeson                        (FromJSON (..), Options (fieldLabelModifier, omitNothingFields),
                                                    ToJSON (..),
                                                    Value (Bool, String),
                                                    defaultOptions, object,
                                                    (.=))
import           Data.Aeson.TH                     (deriveJSON)
import           Data.Default                      (Default (..))
import           Data.Monoid                       ((<>))
import           Data.String                       (IsString (..))
import qualified Data.Text                         as T (pack)
import qualified Data.Text.Lazy.Builder            as B (toLazyText)
import qualified Data.Text.Lazy.Builder.Int        as B (hexadecimal)
import qualified Data.Text.Read                    as R (decimal, hexadecimal)
import           GHC.Generics                      (Generic)

import           Data.String.Extra                 (toLowerFirst)
import           Network.Ethereum.ABI.Prim.Address (Address)
import           Network.Ethereum.ABI.Prim.Bytes   (Bytes, BytesN)
import           Network.Ethereum.Unit

-- | 32 byte type synonym for transaction and block hashes.
type Hash = BytesN 32

-- | Should be viewed as type to representing QUANTITY in Web3 JSON RPC docs
--
--  When encoding QUANTITIES (integers, numbers): encode as hex, prefix with "0x",
--  the most compact representation (slight exception: zero should be represented as "0x0").
--  Examples:
--
--  0x41 (65 in decimal)
--  0x400 (1024 in decimal)
--  WRONG: 0x (should always have at least one digit - zero is "0x0")
--  WRONG: 0x0400 (no leading zeroes allowed)
--  WRONG: ff (must be prefixed 0x)
newtype Quantity = Quantity { unQuantity :: Integer }
    deriving (Read, Num, Real, Enum, Eq, Ord, Generic)

instance Show Quantity where
    show = show . unQuantity

instance IsString Quantity where
    fromString ('0' : 'x' : hex) =
        case R.hexadecimal (T.pack hex) of
            Right (x, "") -> Quantity x
            _             -> error "Unable to parse Quantity!"
    fromString str =
        case R.decimal (T.pack str) of
            Right (x, "") -> Quantity x
            _             -> error "Unable to parse Quantity!"

instance ToJSON Quantity where
    toJSON (Quantity x) =
        let hexValue = B.toLazyText (B.hexadecimal x)
        in  toJSON ("0x" <> hexValue)

instance FromJSON Quantity where
    parseJSON (String v) =
        case R.hexadecimal v of
            Right (x, "") -> return (Quantity x)
            _             -> fail "Unable to parse Quantity"
    parseJSON _ = fail "Quantity may only be parsed from a JSON String"

instance Fractional Quantity where
    (/) a b = Quantity $ div (unQuantity a) (unQuantity b)
    fromRational = Quantity . floor

instance Unit Quantity where
    fromWei = Quantity
    toWei = unQuantity

instance UnitSpec Quantity where
    divider = const 1
    name = const "quantity"

-- | An object with sync status data.
data SyncActive = SyncActive
  { syncStartingBlock :: !Quantity
  -- ^ QUANTITY - The block at which the import started (will only be reset, after the sync reached his head).
  , syncCurrentBlock  :: !Quantity
  -- ^ QUANTITY - The current block, same as eth_blockNumber.
  , syncHighestBlock  :: !Quantity
  -- ^ QUANTITY - The estimated highest block.
  } deriving (Eq, Generic, Show)

$(deriveJSON (defaultOptions
    { fieldLabelModifier = toLowerFirst . drop 4 }) ''SyncActive)

-- | Sync state pulled by low-level call 'eth_syncing'.
data SyncingState = Syncing SyncActive | NotSyncing
    deriving (Eq, Generic, Show)

instance FromJSON SyncingState where
    parseJSON (Bool _) = pure NotSyncing
    parseJSON v        = Syncing <$> parseJSON v

-- | Changes pulled by low-level call 'eth_getFilterChanges', 'eth_getLogs',
-- and 'eth_getFilterLogs'
data Change = Change
  { changeLogIndex         :: !(Maybe Quantity)
  -- ^ QUANTITY - integer of the log index position in the block. null when its pending log.
  , changeTransactionIndex :: !(Maybe Quantity)
  -- ^ QUANTITY - integer of the transactions index position log was created from. null when its pending log.
  , changeTransactionHash  :: !(Maybe Hash)
  -- ^ DATA, 32 Bytes - hash of the transactions this log was created from. null when its pending log.
  , changeBlockHash        :: !(Maybe Hash)
  -- ^ DATA, 32 Bytes - hash of the block where this log was in. null when its pending. null when its pending log.
  , changeBlockNumber      :: !(Maybe Quantity)
  -- ^ QUANTITY - the block number where this log was in. null when its pending. null when its pending log.
  , changeAddress          :: !Address
  -- ^ DATA, 20 Bytes - address from which this log originated.
  , changeData             :: !Bytes
  -- ^ DATA - contains one or more 32 Bytes non-indexed arguments of the log.
  , changeTopics           :: ![BytesN 32]
  -- ^ Array of DATA - Array of 0 to 4 32 Bytes DATA of indexed log arguments.
  -- (In solidity: The first topic is the hash of the signature of the event
  -- (e.g. Deposit(address, bytes32, uint256)), except you declared the event with
  -- the anonymous specifier.)
  } deriving (Eq, Show, Generic)

$(deriveJSON (defaultOptions
    { fieldLabelModifier = toLowerFirst . drop 6 }) ''Change)

-- | The contract call params.
data Call = Call
  { callFrom     :: !(Maybe Address)
  -- ^ DATA, 20 Bytes - The address the transaction is send from.
  , callTo       :: !(Maybe Address)
  -- ^ DATA, 20 Bytes - (optional when creating new contract) The address the transaction is directed to.
  , callGas      :: !(Maybe Quantity)
  -- ^ QUANTITY - (optional, default: 3000000) Integer of the gas provided for the transaction execution. It will return unused gas.
  , callGasPrice :: !(Maybe Quantity)
  -- ^ QUANTITY - (optional, default: To-Be-Determined) Integer of the gasPrice used for each paid gas.
  , callValue    :: !(Maybe Quantity)
  -- ^ QUANTITY - (optional) Integer of the value sent with this transaction.
  , callData     :: !(Maybe Bytes)
  -- ^ DATA - The compiled code of a contract OR the hash of the invoked method signature and encoded parameters.
  , callNonce    :: !(Maybe Quantity)
  -- ^ QUANTITY - (optional) Integer of a nonce. This allows to overwrite your own pending transactions that use the same nonce.
  } deriving (Eq, Show, Generic)

$(deriveJSON (defaultOptions
    { fieldLabelModifier = toLowerFirst . drop 4
    , omitNothingFields = True }) ''Call)

instance Default Call where
    def = Call Nothing Nothing (Just 3000000) Nothing (Just 0) Nothing Nothing

-- | The state of blockchain for contract call.
data DefaultBlock = BlockWithNumber Quantity
                  | Earliest
                  | Latest
                  | Pending
    deriving (Eq, Show, Generic)

instance ToJSON DefaultBlock where
    toJSON (BlockWithNumber bn) = toJSON bn
    toJSON parameter            = toJSON . toLowerFirst . show $ parameter

-- | Low-level event filter data structure.
data Filter e = Filter
  { filterAddress   :: !(Maybe [Address])
  -- ^ DATA|Array, 20 Bytes - (optional) Contract address or a list of addresses from which logs should originate.
  , filterFromBlock :: !DefaultBlock
  -- ^ QUANTITY|TAG - (optional, default: "latest") Integer block number, or "latest" for the last mined block or "pending", "earliest" for not yet mined transactions.
  , filterToBlock   :: !DefaultBlock
  -- ^ QUANTITY|TAG - (optional, default: "latest") Integer block number, or "latest" for the last mined block or "pending", "earliest" for not yet mined transactions.
  , filterTopics    :: !(Maybe [Maybe (BytesN 32)])
  -- ^ Array of DATA, - (optional) Array of 32 Bytes DATA topics. Topics are order-dependent. Each topic can also be an array of DATA with "or" options.
  -- Topics are order-dependent. A transaction with a log with topics [A, B] will be matched by the following topic filters:
  -- * [] "anything"
  -- * [A] "A in first position (and anything after)"
  -- * [null, B] "anything in first position AND B in second position (and anything after)"
  -- * [A, B] "A in first position AND B in second position (and anything after)"
  -- * [[A, B], [A, B]] "(A OR B) in first position AND (A OR B) in second position (and anything after)"
  } deriving (Eq, Show, Generic)

instance ToJSON (Filter e) where
    toJSON f = object [ "address"   .= filterAddress f
                      , "fromBlock" .= filterFromBlock f
                      , "toBlock"   .= filterToBlock f
                      , "topics"    .= filterTopics f ]

instance Ord DefaultBlock where
    compare Pending Pending                         = EQ
    compare Latest Latest                           = EQ
    compare Earliest Earliest                       = EQ
    compare (BlockWithNumber a) (BlockWithNumber b) = compare a b
    compare _ Pending                               = LT
    compare Pending Latest                          = GT
    compare _ Latest                                = LT
    compare Earliest _                              = LT
    compare a b                                     = case compare b a of
                                                        LT -> GT
                                                        GT -> LT
                                                        EQ -> EQ

-- | The Receipt of a Transaction
data TxReceipt = TxReceipt
  { receiptTransactionHash   :: !Hash
  -- ^ DATA, 32 Bytes - hash of the transaction.
  , receiptTransactionIndex  :: !Quantity
  -- ^ QUANTITY - index of the transaction.
  , receiptBlockHash         :: !(Maybe Hash)
  -- ^ DATA, 32 Bytes - hash of the block where this transaction was in. null when its pending.
  , receiptBlockNumber       :: !(Maybe Quantity)
  -- ^ QUANTITY - block number where this transaction was in. null when its pending.
  , receiptCumulativeGasUsed :: !Quantity
  -- ^ QUANTITY - The total amount of gas used when this transaction was executed in the block.
  , receiptGasUsed           :: !Quantity
  -- ^ QUANTITY - The amount of gas used by this specific transaction alone.
  , receiptContractAddress   :: !(Maybe Address)
  -- ^ DATA, 20 Bytes - The contract address created, if the transaction was a contract creation, otherwise null.
  , receiptLogs              :: ![Change]
  -- ^ Array - Array of log objects, which this transaction generated.
  , receiptLogsBloom         :: !Bytes
  -- ^ DATA, 256 Bytes - Bloom filter for light clients to quickly retrieve related logs.
  , receiptStatus            :: !(Maybe Quantity)
  -- ^ QUANTITY either 1 (success) or 0 (failure)
  } deriving (Show, Generic)

$(deriveJSON (defaultOptions
    { fieldLabelModifier = toLowerFirst . drop 7 }) ''TxReceipt)

-- | Transaction information.
data Transaction = Transaction
  { txHash             :: !Hash
  -- ^ DATA, 32 Bytes - hash of the transaction.
  , txNonce            :: !Quantity
  -- ^ QUANTITY - the number of transactions made by the sender prior to this one.
  , txBlockHash        :: !(Maybe Hash)
  -- ^ DATA, 32 Bytes - hash of the block where this transaction was in. null when its pending.
  , txBlockNumber      :: !(Maybe Quantity)
  -- ^ QUANTITY - block number where this transaction was in. null when its pending.
  , txTransactionIndex :: !(Maybe Quantity)
  -- ^ QUANTITY - integer of the transactions index position in the block. null when its pending.
  , txFrom             :: !Address
  -- ^ DATA, 20 Bytes - address of the sender.
  , txTo               :: !(Maybe Address)
  -- ^ DATA, 20 Bytes - address of the receiver. null when its a contract creation transaction.
  , txValue            :: !Quantity
  -- ^ QUANTITY - value transferred in Wei.
  , txGasPrice         :: !Quantity
  -- ^ QUANTITY - gas price provided by the sender in Wei.
  , txGas              :: !Quantity
  -- ^ QUANTITY - gas provided by the sender.
  , txInput            :: !Bytes
  -- ^ DATA - the data send along with the transaction.
  } deriving (Eq, Show, Generic)

$(deriveJSON (defaultOptions
    { fieldLabelModifier = toLowerFirst . drop 2 }) ''Transaction)

-- | Block information.
data Block = Block
  { blockNumber           :: !(Maybe Quantity)
  -- ^ QUANTITY - the block number. null when its pending block.
  , blockHash             :: !(Maybe Hash)
  -- ^ DATA, 32 Bytes - hash of the block. null when its pending block.
  , blockParentHash       :: !Hash
  -- ^ DATA, 32 Bytes - hash of the parent block.
  , blockNonce            :: !(Maybe Bytes)
  -- ^ DATA, 8 Bytes - hash of the generated proof-of-work. null when its pending block.
  , blockSha3Uncles       :: !(BytesN 32)
  -- ^ DATA, 32 Bytes - SHA3 of the uncles data in the block.
  , blockLogsBloom        :: !(Maybe Bytes)
  -- ^ DATA, 256 Bytes - the bloom filter for the logs of the block. null when its pending block.
  , blockTransactionsRoot :: !(BytesN 32)
  -- ^ DATA, 32 Bytes - the root of the transaction trie of the block.
  , blockStateRoot        :: !(BytesN 32)
  -- ^ DATA, 32 Bytes - the root of the final state trie of the block.
  , blockReceiptRoot      :: !(Maybe (BytesN 32))
  -- ^ DATA, 32 Bytes - the root of the receipts trie of the block.
  , blockMiner            :: !Address
  -- ^ DATA, 20 Bytes - the address of the beneficiary to whom the mining rewards were given.
  , blockDifficulty       :: !Quantity
  -- ^ QUANTITY - integer of the difficulty for this block.
  , blockTotalDifficulty  :: !Quantity
  -- ^ QUANTITY - integer of the total difficulty of the chain until this block.
  , blockExtraData        :: !Bytes
  -- ^ DATA - the "extra data" field of this block.
  , blockSize             :: !Quantity
  -- ^ QUANTITY - integer the size of this block in bytes.
  , blockGasLimit         :: !Quantity
  -- ^ QUANTITY - the maximum gas allowed in this block.
  , blockGasUsed          :: !Quantity
  -- ^ QUANTITY - the total used gas by all transactions in this block.
  , blockTimestamp        :: !Quantity
  -- ^ QUANTITY - the unix timestamp for when the block was collated.
  , blockTransactions     :: ![Transaction]
  -- ^ Array of transaction objects.
  , blockUncles           :: ![Hash]
  -- ^ Array - Array of uncle hashes.
  } deriving (Show, Generic)

$(deriveJSON (defaultOptions
    { fieldLabelModifier = toLowerFirst . drop 5 }) ''Block)
