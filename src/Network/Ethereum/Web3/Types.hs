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

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Default
import           Data.Monoid                       ((<>))
import           Data.Ord                          (Down (..))
import           Data.String                       (IsString (..))
import qualified Data.Text                         as T
import qualified Data.Text.Lazy.Builder            as B
import qualified Data.Text.Lazy.Builder.Int        as B
import qualified Data.Text.Read                    as R
import           GHC.Generics                      (Generic)

import           Data.String.Extra                 (toLowerFirst)
import           Network.Ethereum.ABI.Prim.Address (Address)
import           Network.Ethereum.ABI.Prim.Bytes   (Bytes)
import           Network.Ethereum.Unit

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
    deriving (Show, Read, Num, Real, Enum, Eq, Ord, Generic)

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

newtype BlockNumber = BlockNumber Integer deriving (Eq, Show, Generic, Ord, Read, Num)

instance FromJSON BlockNumber where
    parseJSON (String v) =
        case R.hexadecimal v of
            Right (x, "") -> return (BlockNumber x)
            _             -> fail "Unable to parse BlockNumber!"
    parseJSON _ = fail "The string is required!"

instance ToJSON BlockNumber where
    toJSON (BlockNumber x) =
        let hexValue = B.toLazyText (B.hexadecimal x)
        in  toJSON ("0x" <> hexValue)


data SyncActive = SyncActive { syncStartingBlock :: BlockNumber
                             , syncCurrentBlock  :: BlockNumber
                             , syncHighestBlock  :: BlockNumber
                             } deriving (Eq, Generic, Show)
$(deriveJSON (defaultOptions { fieldLabelModifier = toLowerFirst . drop 4 }) ''SyncActive)

data SyncingState = Syncing SyncActive | NotSyncing deriving (Eq, Generic, Show)

instance FromJSON SyncingState where
    parseJSON (Bool _) = pure NotSyncing
    parseJSON v        = Syncing <$> parseJSON v


-- | Event filter identifier
newtype FilterId = FilterId Integer
  deriving (Show, Eq, Ord, Generic)

instance FromJSON FilterId where
    parseJSON (String v) =
        case R.hexadecimal v of
            Right (x, "") -> return (FilterId x)
            _             -> fail "Unable to parse FilterId!"
    parseJSON _ = fail "The string is required!"

instance ToJSON FilterId where
    toJSON (FilterId x) =
        let hexValue = B.toLazyText (B.hexadecimal x)
        in  toJSON ("0x" <> hexValue)

-- | Changes pulled by low-level call 'eth_getFilterChanges', 'eth_getLogs',
-- and 'eth_getFilterLogs'
data Change = Change
  { changeLogIndex         :: !Quantity
  , changeTransactionIndex :: !Quantity
  , changeTransactionHash  :: !Bytes
  , changeBlockHash        :: !Bytes
  , changeBlockNumber      :: !BlockNumber
  , changeAddress          :: !Address
  , changeData             :: !Bytes
  , changeTopics           :: ![Bytes]
  } deriving (Show, Generic)

$(deriveJSON (defaultOptions
    { fieldLabelModifier = toLowerFirst . drop 6 }) ''Change)

-- | The contract call params
data Call = Call
  { callFrom     :: !(Maybe Address)
  , callTo       :: !(Maybe Address)
  , callGas      :: !(Maybe Quantity)
  , callGasPrice :: !(Maybe Quantity)
  , callValue    :: !(Maybe Quantity)  -- expressed in wei
  , callData     :: !(Maybe Bytes)
  } deriving (Show, Generic)

$(deriveJSON (defaultOptions
    { fieldLabelModifier = toLowerFirst . drop 4
    , omitNothingFields = True }) ''Call)

instance Default Call where
    def = Call Nothing Nothing (Just 3000000) Nothing (Just 0) Nothing


-- | The contract call mode describe used state: latest or pending
data DefaultBlock = BlockWithNumber BlockNumber | Earliest | Latest | Pending
  deriving (Show, Eq)

instance ToJSON DefaultBlock where
    toJSON (BlockWithNumber bn) = toJSON bn
    toJSON parameter            = toJSON . toLowerFirst . show $ parameter

-- | Low-level event filter data structure
data Filter e = Filter
  { filterAddress   :: !(Maybe Address)
  , filterTopics    :: !(Maybe [Maybe Bytes])
  , filterFromBlock :: !DefaultBlock
  , filterToBlock   :: !DefaultBlock
  } deriving (Show, Generic)


instance ToJSON (Filter e) where
  toJSON f = object [ "address" .= filterAddress f
                    , "topics" .= filterTopics f
                    , "fromBlock" .= filterFromBlock f
                    , "toBlock" .= filterToBlock f
                    ]

instance Ord DefaultBlock where
    compare Pending Pending                         = EQ
    compare Latest Latest                           = EQ
    compare Earliest Earliest                       = EQ
    compare (BlockWithNumber a) (BlockWithNumber b) = compare a b
    compare _ Pending                               = LT
    compare Pending Latest                          = GT
    compare _ Latest                                = LT
    compare Earliest _                              = LT
    compare a b                                     = compare (Down b) (Down a)


-- | Transaction information
data Transaction = Transaction
  { txHash             :: !Bytes
  -- ^ DATA, 32 Bytes - hash of the transaction.
  , txNonce            :: !Quantity
  -- ^ QUANTITY - the number of transactions made by the sender prior to this one.
  , txBlockHash        :: !Bytes
  -- ^ DATA, 32 Bytes - hash of the block where this transaction was in. null when its pending.
  , txBlockNumber      :: !BlockNumber
  -- ^ QUANTITY - block number where this transaction was in. null when its pending.
  , txTransactionIndex :: !Quantity
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
  } deriving (Show, Generic)

$(deriveJSON (defaultOptions
    { fieldLabelModifier = toLowerFirst . drop 2 }) ''Transaction)

-- | Block information
data Block = Block
  { blockNumber           :: !BlockNumber
  -- ^ QUANTITY - the block number. null when its pending block.
  , blockHash             :: !Bytes
  -- ^ DATA, 32 Bytes - hash of the block. null when its pending block.
  , blockParentHash       :: !Bytes
  -- ^ DATA, 32 Bytes - hash of the parent block.
  , blockNonce            :: !(Maybe Bytes)
  -- ^ DATA, 8 Bytes - hash of the generated proof-of-work. null when its pending block.
  , blockSha3Uncles       :: !Bytes
  -- ^ DATA, 32 Bytes - SHA3 of the uncles data in the block.
  , blockLogsBloom        :: !Bytes
  -- ^ DATA, 256 Bytes - the bloom filter for the logs of the block. null when its pending block.
  , blockTransactionsRoot :: !Bytes
  -- ^ DATA, 32 Bytes - the root of the transaction trie of the block.
  , blockStateRoot        :: !Bytes
  -- ^ DATA, 32 Bytes - the root of the final state trie of the block.
  , blockReceiptRoot      :: !(Maybe Bytes)
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
  , blockUncles           :: ![Bytes]
  -- ^ Array - Array of uncle hashes.
  } deriving (Show, Generic)

$(deriveJSON (defaultOptions
    { fieldLabelModifier = toLowerFirst . drop 5 }) ''Block)

type TxHash = Bytes
