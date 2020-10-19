{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

-- |
-- Module      :  Network.Polkadot.Api.Types
-- Copyright   :  Aleksandr Krupenkin 2016-2020
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Polkadot JSON-RPC types.
--

module Network.Polkadot.Api.Types where

import           Data.Aeson               (FromJSON (..),
                                           Options (fieldLabelModifier),
                                           ToJSON (..), Value (String),
                                           defaultOptions)
import           Data.Aeson.TH            (deriveJSON)
import           Data.ByteArray.HexString (HexString)
import           Data.Char                (toLower)
import           Data.Text                (Text)
import           Data.Word                (Word32, Word64, Word8)
import           GHC.Generics             (Generic)
import           Lens.Micro               (over, _head)

-- | The role the node is running as.
data NodeRole = Full
    | LightClient
    | Authority
    | Sentry
    deriving (Eq, Generic, Show)

$(deriveJSON defaultOptions ''NodeRole)

-- | Type op a chain.
data ChainType = Development
    | Local
    | Live
    | Custom Text
    deriving (Eq, Generic, Show)

instance FromJSON ChainType where
    parseJSON (String v) = return $ case v of
        "Development" -> Development
        "Local"       -> Local
        "Live"        -> Live
        custom_name   -> Custom custom_name
    parseJSON _ = fail "ChainType should be a JSON String"

instance ToJSON ChainType where
    toJSON (Custom v) = toJSON v
    toJSON v          = toJSON (show v)

-- | System health struct returned by the RPC
data Health = Health
    { healthPeers           :: Int
    -- ^ Number of connected peers.
    , healthIsSyncing       :: Bool
    -- ^ Is the node syncing.
    , healthShouldHavePeers :: Bool
    -- ^ Should this node have any peers.
    }
    deriving (Eq, Generic, Show)

$(deriveJSON (defaultOptions
    { fieldLabelModifier = over _head toLower . drop 6 }) ''Health)

-- | Network Peer information.
data PeerInfo = PeerInfo
    { peerInfoPeerId          :: Text
    -- ^ Peer ID
    , peerInfoRoles           :: [NodeRole]
    -- ^ Roles
    , peerInfoProtocolVersion :: Int
    -- ^ Protocol version.
    , peerInfoBestHash        :: Text
    -- ^ Peer best block hash
    , peerInfoBestNumber      :: Int
    -- ^ Peer best block number
    }
    deriving (Eq, Generic, Show)

$(deriveJSON (defaultOptions
    { fieldLabelModifier = over _head toLower . drop 8 }) ''PeerInfo)

-- | Executes a call to a contract.
data ContractCall = ContractCall
    { callOrigin    :: HexString
    , callDest      :: HexString
    , callValue     :: Integer
    , callGasLimit  :: Integer
    , callInputData :: HexString
    }
    deriving (Eq, Generic, Show)

$(deriveJSON (defaultOptions
    { fieldLabelModifier = over _head toLower . drop 4 }) ''ContractCall)

-- | A result of execution of a contract.
data ContractExecResult = SuccessExec
    { execStatus :: Word8
    -- ^ Status code returned by contract.
    , execData   :: Maybe HexString
    -- ^ Output data returned by contract. Can be empty.
    }
    | ExecResultError
    deriving (Eq, Generic, Show)

$(deriveJSON (defaultOptions
    { fieldLabelModifier = over _head toLower . drop 4 }) ''ContractExecResult)

-- | ReadProof struct returned by RPC.
data ReadProof = ReadProof
    { readProofAt    :: HexString
    -- ^ Block hash used to generate the proof.
    , readProofProof :: [HexString]
    -- ^ A proof used to prove that storage entries are included in the storage trie.
    }
    deriving (Eq, Generic, Show)

$(deriveJSON (defaultOptions
    { fieldLabelModifier = over _head toLower . drop 9 }) ''ReadProof)

-- | Runtime version.
-- This should not be thought of as classic Semver (major/minor/tiny).
-- This triplet have different semantics and mis-interpretation could cause problems.
-- In particular: bug fixes should result in an increment of `spec_version` and possibly `authoring_version`,
-- absolutely not `impl_version` since they change the semantics of the runtime.
data RuntimeVersion = RuntimeVersion
    { runtimeSpecName           :: Text
    -- ^ Identifies the different Substrate runtimes.
    , runtimeImplName           :: Text
    -- ^ Name of the implementation of the spec.
    , runtimeAuthoringVersion   :: Word32
    -- ^ `authoring_version` is the version of the authorship interface.
    , runtimeSpecVersion        :: Word32
    -- ^ Version of the runtime specification.
    , runtimeImplVersion        :: Word32
    -- ^ Version of the implementation of the specification.
    , runtimeApis               :: [(HexString, Word32)]
    -- ^ List of supported API "features" along with their versions.
    , runtimeTransactionVersion :: Word32
    -- ^ All existing dispatches are fully compatible when this number doesn't change.
    }
    deriving (Eq, Generic, Show)

$(deriveJSON (defaultOptions
    { fieldLabelModifier = over _head toLower . drop 7 }) ''RuntimeVersion)

-- | Type of supported offchain storages.
--
-- 1: persistent storage is non-revertible and not fork-aware;
-- 2: local storage is revertible and fork-aware.
type StorageKind = Word8

-- | Storage changes.
data StorageChangeSet = StorageChangeSet
    { storageBlock   :: HexString
    -- ^ Block hash.
    , storageChanges :: [(HexString, Maybe HexString)]
    -- ^ A list of changes.
    }

$(deriveJSON (defaultOptions
    { fieldLabelModifier = over _head toLower . drop 7 }) ''StorageChangeSet)

-- | Numeric range of transaction weight.
type Weight = Word64

-- | Generalized group of dispatch types.
data DispatchClass = Normal
    | Operational
    | Mandatory
    deriving (Eq, Generic, Show)

$(deriveJSON defaultOptions ''DispatchClass)

-- | Some information related to a dispatchable that can be queried from the runtime.
data RuntimeDispatchInfo = RuntimeDispatchInfo
    { dispatchWeight     :: Weight
    -- ^ Weight of this dispatch.
    , dispatchClass      :: DispatchClass
    -- ^ Class of this dispatch.
    , dispatchPartialFee :: Integer
    -- ^ The partial inclusion fee of this dispatch.
    }
    deriving (Eq, Generic, Show)

$(deriveJSON (defaultOptions
    { fieldLabelModifier = over _head toLower . drop 8 }) ''RuntimeDispatchInfo)

-- | Auxiliary data associated with an imported block result.
data ImportedAux = ImportedAux
    { auxHeaderOnly                 :: Bool
    -- ^ Only the header has been imported. Block body verification was skipped.
    , auxClearJustificationRequests :: Bool
    -- ^ Clear all pending justification requests.
    , auxNeedsJustification         :: Bool
    -- ^ Request a justification for the given block.
    , auxBadJustification           :: Bool
    -- ^ Received a bad justification.
    , auxNeedsFinalityProof         :: Bool
    -- ^ Request a finality proof for the given block.
    , auxIsNewBest                  :: Bool
    -- ^ Whether the block that was imported is the new best block.
    }
    deriving (Eq, Generic, Show)

$(deriveJSON (defaultOptions
    { fieldLabelModifier = over _head toLower . drop 3 }) ''ImportedAux)

data CreatedBlock = CreatedBlock
    { createdBlockHash :: HexString
    , createdBlockAux  :: ImportedAux
    }
    deriving (Eq, Generic, Show)

$(deriveJSON (defaultOptions
    { fieldLabelModifier = over _head toLower . drop 12 }) ''CreatedBlock)

-- | Abstraction over a block header for a substrate chain.
data Header = Header
    { headerParentHash     :: HexString
    -- ^ The parent hash.
    , headerNumber         :: Int
    -- ^ The block number.
    , headerStateRoot      :: HexString
    -- ^ The state trie merkle root
    , headerExtrinsicsRoot :: HexString
    -- ^ The merkle root of the extrinsics.
    , headerDigest         :: HexString
    -- ^ A chain-specific digest of data useful for light clients or referencing auxiliary data.
    }
    deriving (Eq, Generic, Show)

$(deriveJSON (defaultOptions
    { fieldLabelModifier = over _head toLower . drop 5 }) ''Header)

-- | Abstraction over a substrate block.
data Block = Block
    { blockHeader     :: Header
    -- ^ The block header.
    , blockExtrinsics :: [HexString]
    -- ^ The accompanying extrinsics.
    }
    deriving (Eq, Generic, Show)

$(deriveJSON (defaultOptions
    { fieldLabelModifier = over _head toLower . drop 5 }) ''Block)

-- | Abstraction over a substrate block and justification.
data SignedBlock = SignedBlock
    { signedBlock         :: Block
    -- ^ Full block.
    , signedJustification :: Maybe HexString
    -- ^ Block justification.
    }
    deriving (Eq, Generic, Show)

$(deriveJSON (defaultOptions
    { fieldLabelModifier = over _head toLower . drop 6 }) ''SignedBlock)
