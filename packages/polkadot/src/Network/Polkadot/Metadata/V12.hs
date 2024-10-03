{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :  Network.Polkadot.Metadata.V12
-- Copyright   :  Aleksandr Krupenkin 2016-2024
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Metadata version 12 definitions.
--

module Network.Polkadot.Metadata.V12 where

import           Codec.Scale                   (Decode, Encode, Generic)
import           Data.Aeson                    (Options (fieldLabelModifier),
                                                defaultOptions)
import           Data.Aeson.TH                 (deriveJSON)
import           Data.Char                     (toLower)
import           Data.Text                     (Text)
import           Data.Word                     (Word8)
import qualified GHC.Generics                  as GHC (Generic)
import           Lens.Micro                    (_head, over)

import qualified Network.Polkadot.Metadata.V11 as V11

type ExtrinsicMetadata = V11.ExtrinsicMetadata
type StorageMetadata = V11.StorageMetadata
type FunctionMetadata = V11.FunctionMetadata
type EventMetadata = V11.EventMetadata
type ModuleConstantMetadata = V11.ModuleConstantMetadata
type ErrorMetadata = V11.ErrorMetadata
type MapType = V11.MapType
type DoubleMapType = V11.DoubleMapType
type StorageHasher = V11.StorageHasher
type StorageEntryModifier = V11.StorageEntryModifier

data ModuleMetadata = ModuleMetadata
    { moduleName      :: !Text
    , moduleStorage   :: !(Maybe StorageMetadata)
    , moduleCalls     :: !(Maybe [FunctionMetadata])
    , moduleEvents    :: !(Maybe [EventMetadata])
    , moduleConstants :: ![ModuleConstantMetadata]
    , moduleErrors    :: ![ErrorMetadata]
    , moduleIndex     :: !Word8
    } deriving (Eq, Show, Generic, GHC.Generic, Encode, Decode)

$(deriveJSON (defaultOptions
    { fieldLabelModifier = over _head toLower . drop 6 }) ''ModuleMetadata)

data Metadata = Metadata
    { modules   :: ![ModuleMetadata]
    , extrinsic :: !ExtrinsicMetadata
    } deriving (Eq, Show, Generic, GHC.Generic, Encode, Decode)

$(deriveJSON defaultOptions ''Metadata)
