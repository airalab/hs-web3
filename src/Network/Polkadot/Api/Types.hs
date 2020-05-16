{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

-- |
-- Module      :  Network.Polkadot.Api.Types
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Polkadot JSON-RPC types.
--

module Network.Polkadot.Api.Types where

import           Data.Aeson        (FromJSON (..), Options (fieldLabelModifier, omitNothingFields),
                                    ToJSON (..), Value (Bool, String),
                                    defaultOptions, object, (.=))
import           Data.Aeson.TH     (deriveJSON)
import           Data.Text         (Text)
import           GHC.Generics      (Generic)

import           Data.String.Extra (toLowerFirst)

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
    { fieldLabelModifier = toLowerFirst . drop 6 }) ''Health)

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
    { fieldLabelModifier = toLowerFirst . drop 8 }) ''PeerInfo)

data ContractCallRequest = ContractCallRequest
    deriving (Eq, Generic, Show)

$(deriveJSON (defaultOptions
    { fieldLabelModifier = toLowerFirst . drop 8 }) ''ContractCallRequest)

data ContractExecResult = ContractExecResult
    deriving (Eq, Generic, Show)

$(deriveJSON (defaultOptions
    { fieldLabelModifier = toLowerFirst . drop 8 }) ''ContractExecResult)
