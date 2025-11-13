{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

-- |
-- Module      :  Network.Ethereum.Api.Types
-- Copyright   :  Aleksandr Krupenkin 2016-2024
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Ethereum generic JSON-RPC types.
--

module Network.Ethereum.Api.Types where

import           Data.Aeson                 (FromJSON (..),
                                             Options (fieldLabelModifier, omitNothingFields),
                                             ToJSON (..), Value (Bool, String),
                                             defaultOptions, object, (.=))
import           Data.Aeson.TH              (deriveJSON)
import           Data.ByteArray.HexString   (HexString)
import           Data.Char                  (toLower)
import           Data.Default               (Default (..))
import           Data.Solidity.Prim.Address (Address)
import           Data.String                (IsString (..))
import qualified Data.Text                  as T (pack)
import qualified Data.Text.Lazy.Builder     as B (toLazyText)
import qualified Data.Text.Lazy.Builder.Int as B (hexadecimal)
import qualified Data.Text.Read             as R (decimal, hexadecimal)
import           Data.Word                  (Word8)
import           GHC.Generics               (Generic)
import           Lens.Micro                 (_head, over)

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
    deriving (Num, Real, Integral, Enum, Eq, Ord, Generic)

instance Show Quantity where
    show = show . unQuantity

instance IsString Quantity where
    fromString ('0' : 'x' : hex) =
        case R.hexadecimal (T.pack hex) of
            Right (x, "") -> Quantity x
            _             -> error ("Quantity " ++ show hex ++ " is not valid hex")
    fromString num =
        case R.decimal (T.pack num) of
            Right (x, "") -> Quantity x
            _             -> error ("Quantity " ++ show num ++ " is not valid decimal")

instance ToJSON Quantity where
    toJSON (Quantity x) =
        let hexValue = B.toLazyText (B.hexadecimal x)
        in  toJSON ("0x" <> hexValue)

instance FromJSON Quantity where
    parseJSON (String v) =
        case R.hexadecimal v of
            Right (x, "") -> return (Quantity x)
            _             -> fail $ "Quantity " ++ show v <> " is not valid hex"
    parseJSON _ = fail "Quantity should be a JSON String"

-- | Type representing a Byte in Web3 JSON RPC docs ("#/components/schemas/byte")
--
-- The encoding is similar to Quantity, encoding as hex string.
newtype Byte = Byte { unByte :: Word8 }
    deriving (Num, Real, Integral, Enum, Eq, Ord, Generic)

instance Show Byte where
    show = show . unByte

instance IsString Byte where
    fromString ('0' : 'x' : hex) =
       case R.hexadecimal (T.pack hex) of
            Right (x, "")
                | toInteger (minBound :: Word8) <= x && x <= toInteger (maxBound :: Word8) -> Byte (fromInteger x)
                | otherwise -> error ("Byte " ++ show hex ++ "is not within bounds")
            _               -> error ("Byte " ++ show hex ++ " is not valid hex")

    fromString num =
        case R.decimal (T.pack num) of
            Right (x, "")
                | toInteger (minBound :: Word8) <= x && x <= toInteger (maxBound :: Word8) -> Byte (fromInteger x)
                | otherwise -> error ("Byte " ++ show num ++ "is not within bounds")
            _             -> error ("Byte " ++ show num ++ " is not valid decimal")

instance ToJSON Byte where
    toJSON (Byte x) =
        let hexValue = B.toLazyText (B.hexadecimal x)
        in  toJSON ("0x" <> hexValue)

instance FromJSON Byte where
    parseJSON (String v) =
        case R.hexadecimal v of
            Right (x, "") -> return (Byte x)
            _             -> fail $ "Byte " ++ show v <> " is not valid hex"
    parseJSON _ = fail "Byte should be a JSON String"


-- | An object with sync status data.
data SyncActive = SyncActive
    { syncStartingBlock :: !Quantity
    -- ^ QUANTITY - The block at which the import started (will only be reset, after the sync reached his head).
    , syncCurrentBlock  :: !Quantity
    -- ^ QUANTITY - The current block, same as eth_blockNumber.
    , syncHighestBlock  :: !Quantity
    -- ^ QUANTITY - The estimated highest block.
    }
    deriving (Eq, Generic, Show)

$(deriveJSON (defaultOptions
    { fieldLabelModifier = over _head toLower . drop 4 }) ''SyncActive)

-- | Sync state pulled by low-level call 'eth_syncing'.
data SyncingState = Syncing SyncActive
    | NotSyncing
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
    , changeTransactionHash  :: !(Maybe HexString)
    -- ^ DATA, 32 Bytes - hash of the transactions this log was created from. null when its pending log.
    , changeBlockHash        :: !(Maybe HexString)
    -- ^ DATA, 32 Bytes - hash of the block where this log was in. null when its pending. null when its pending log.
    , changeBlockNumber      :: !(Maybe Quantity)
    -- ^ QUANTITY - the block number where this log was in. null when its pending. null when its pending log.
    , changeAddress          :: !Address
    -- ^ DATA, 20 Bytes - address from which this log originated.
    , changeData             :: !HexString
    -- ^ DATA - contains one or more 32 Bytes non-indexed arguments of the log.
    , changeTopics           :: ![HexString]
    -- ^ Array of DATA - Array of 0 to 4 32 Bytes DATA of indexed log arguments.
    }
    deriving (Eq, Show, Generic)

$(deriveJSON (defaultOptions
    { fieldLabelModifier = over _head toLower . drop 6 }) ''Change)

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
    , callData     :: !(Maybe HexString)
    -- ^ DATA - The compiled code of a contract OR the hash of the invoked method signature and encoded parameters.
    , callNonce    :: !(Maybe Quantity)
    -- ^ QUANTITY - (optional) Integer of a nonce. This allows to overwrite your own pending transactions that use the same nonce.
    }
    deriving (Eq, Show, Generic)

$(deriveJSON (defaultOptions
    { fieldLabelModifier = over _head toLower . drop 4
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
    toJSON parameter            = toJSON . over _head toLower . show $ parameter

-- | Low-level event filter data structure.
data Filter e = Filter
    { filterAddress   :: !(Maybe [Address])
    -- ^ DATA|Array, 20 Bytes - (optional) Contract address or a list of addresses from which logs should originate.
    , filterFromBlock :: !DefaultBlock
    -- ^ QUANTITY|TAG - (optional, default: "latest") Integer block number, or "latest" for the last mined block or "pending", "earliest" for not yet mined transactions.
    , filterToBlock   :: !DefaultBlock
    -- ^ QUANTITY|TAG - (optional, default: "latest") Integer block number, or "latest" for the last mined block or "pending", "earliest" for not yet mined transactions.
    , filterTopics    :: !(Maybe [Maybe HexString])
    -- ^ Array of DATA, - (optional) Array of 32 Bytes DATA topics. Topics are order-dependent. Each topic can also be an array of DATA with "or" options.
    }
    deriving (Eq, Show, Generic)

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
    { receiptType :: !(Maybe Byte)
    -- ^ BYTE - type of the transaction 0x00 for legacy transactions, 0x01 EIP-2930, 0x02 EIP-1559
    , receiptTransactionHash   :: !HexString
    -- ^ DATA, 32 Bytes - hash of the transaction.
    , receiptTransactionIndex  :: !Quantity
    -- ^ QUANTITY - index of the transaction.
    , receiptBlockHash         :: !HexString
    -- ^ DATA, 32 Bytes - hash of the block where this transaction was in. 
    , receiptBlockNumber       :: !Quantity
    -- ^ QUANTITY - block number where this transaction was in.
    , receiptFrom              :: !Address
    -- ^ DATA, 20 Bytes - the address of the sender
    , receiptTo                :: !(Maybe Address)
    -- ^ DATA, 20 Bytes - The address of the receiver. null when the transaction is a contract creation transaction.
    , receiptCumulativeGasUsed :: !Quantity
    -- ^ QUANTITY - The total amount of gas used when this transaction was executed in the block.
    , receiptGasUsed           :: !Quantity
    -- ^ QUANTITY - The amount of gas used by this specific transaction alone.
    , receiptBlobGasUsed       :: !(Maybe Quantity)
    -- ^ QUANTITY - The amount of blob gas used for this specific transaction. Only specified for blob transactions as defined by EIP-4844.
    , receiptContractAddress   :: !(Maybe Address)
    -- ^ DATA, 20 Bytes - The contract address created, if the transaction was a contract creation, otherwise null.
    , receiptLogs              :: ![Change]
    -- ^ Array - Array of log objects, which this transaction generated.
    , receiptLogsBloom         :: !HexString
    -- ^ DATA, 256 Bytes - Bloom filter for light clients to quickly retrieve related logs.
    , receiptRoot              :: !(Maybe HexString)
    -- ^ DATA, 32 Bytes - The post-transaction state root. Only specified for transactions included before the Byzantium upgrade.
    , receiptStatus            :: !(Maybe Quantity)
    -- ^ QUANTITY either 1 (success) or 0 (failure)
    , receiptEffectiveGasPrice :: !Quantity
    -- ^ QUANTITY - The actual value per gas deducted from the sender's account. Before EIP-1559, this is equal to the transaction's gas price. After, it is equal to baseFeePerGas + min(maxFeePerGas - baseFeePerGas, maxPriorityFeePerGas).
    , blobGasPrice             :: !(Maybe Quantity)
    -- ^ QUANTITY - The actual value per gas deducted from the sender's account for blob gas. Only specified for blob transactions as defined by EIP-4844.
    }
    deriving (Show, Generic)

$(deriveJSON (defaultOptions
    { fieldLabelModifier = over _head toLower . drop 7 }) ''TxReceipt)

-- | Transaction information.
data Transaction = Transaction
    { txHash             :: !HexString
    -- ^ DATA, 32 Bytes - hash of the transaction.
    , txNonce            :: !Quantity
    -- ^ QUANTITY - the number of transactions made by the sender prior to this one.
    , txBlockHash        :: !(Maybe HexString)
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
    , txInput            :: !HexString
    -- ^ DATA - the data send along with the transaction.
    }
    deriving (Eq, Show, Generic)

$(deriveJSON (defaultOptions
    { fieldLabelModifier = over _head toLower . drop 2 }) ''Transaction)

-- | Block information with full transactions.
type Block = BlockT Transaction

-- | Block information.
-- It is parameterized by the type of transactions stored in the block because
-- sometimes only TX hashes may be stored.
data BlockT tx = Block
    { blockNumber           :: !(Maybe Quantity)
    -- ^ QUANTITY - the block number. null when its pending block.
    , blockHash             :: !(Maybe HexString)
    -- ^ DATA, 32 Bytes - hash of the block. null when its pending block.
    , blockParentHash       :: !HexString
    -- ^ DATA, 32 Bytes - hash of the parent block.
    , blockNonce            :: !(Maybe HexString)
    -- ^ DATA, 8 Bytes - hash of the generated proof-of-work. null when its pending block.
    , blockSha3Uncles       :: !HexString
    -- ^ DATA, 32 Bytes - SHA3 of the uncles data in the block.
    , blockLogsBloom        :: !(Maybe HexString)
    -- ^ DATA, 256 Bytes - the bloom filter for the logs of the block. null when its pending block.
    , blockTransactionsRoot :: !HexString
    -- ^ DATA, 32 Bytes - the root of the transaction trie of the block.
    , blockStateRoot        :: !HexString
    -- ^ DATA, 32 Bytes - the root of the final state trie of the block.
    , blockReceiptsRoot     :: !(Maybe HexString)
    -- ^ DATA, 32 Bytes - the root of the receipts trie of the block.
    , blockMiner            :: !Address
    -- ^ DATA, 20 Bytes - the address of the beneficiary to whom the mining rewards were given.
    , blockDifficulty       :: !Quantity
    -- ^ QUANTITY - integer of the difficulty for this block.
    , blockTotalDifficulty  :: !(Maybe Quantity)
    -- ^ QUANTITY - integer of the total difficulty of the chain until this block.
    , blockExtraData        :: !HexString
    -- ^ DATA - the "extra data" field of this block.
    , blockSize             :: !Quantity
    -- ^ QUANTITY - integer the size of this block in bytes.
    , blockGasLimit         :: !Quantity
    -- ^ QUANTITY - the maximum gas allowed in this block.
    , blockGasUsed          :: !Quantity
    -- ^ QUANTITY - the total used gas by all transactions in this block.
    , blockTimestamp        :: !Quantity
    -- ^ QUANTITY - the unix timestamp for when the block was collated.
    , blockTransactions     :: ![tx]
    -- ^ Array of transaction objects.
    , blockUncles           :: ![HexString]
    -- ^ Array - Array of uncle hashes.
    }
    deriving (Show, Generic)

$(deriveJSON (defaultOptions
    { fieldLabelModifier = over _head toLower . drop 5 }) ''BlockT)
