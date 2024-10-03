{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :  Network.Polkadot.Metadata.V13
-- Copyright   :  Aleksandr Krupenkin 2016-2024
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Metadata version 13 definitions.
--

module Network.Polkadot.Metadata.V13 where

import           Codec.Scale                    (Decode, Encode, Generic)
import           Data.Aeson                     (Options (constructorTagModifier, fieldLabelModifier, sumEncoding),
                                                 SumEncoding (ObjectWithSingleField),
                                                 defaultOptions)
import           Data.Aeson.TH                  (deriveJSON)
import           Data.ByteArray.HexString       (HexString)
import           Data.Char                      (toLower)
import           Data.Text                      (Text)
import           Data.Word                      (Word8)
import qualified GHC.Generics                   as GHC (Generic)
import           Lens.Micro                     (_head, over)

import           Network.Polkadot.Metadata.Type (Type)
import qualified Network.Polkadot.Metadata.V12  as V12

type ExtrinsicMetadata = V12.ExtrinsicMetadata
type FunctionMetadata = V12.FunctionMetadata
type EventMetadata = V12.EventMetadata
type ModuleConstantMetadata = V12.ModuleConstantMetadata
type ErrorMetadata = V12.ErrorMetadata
type StorageHasher = V12.StorageHasher
type MapType = V12.MapType
type DoubleMapType = V12.DoubleMapType

data NMapType = NMapType
    { nmapKeyVec  :: ![Type]
    , nmapHashers :: ![StorageHasher]
    , nmapValue   :: !Type
    } deriving (Eq, Show, Generic, GHC.Generic, Encode, Decode)

$(deriveJSON (defaultOptions
    { fieldLabelModifier = over _head toLower . drop 4 }) ''NMapType)

data StorageEntryType
    = Plain !Type
    | Map !MapType
    | DoubleMap !DoubleMapType
    | NMap !NMapType
    deriving (Eq, Show, Generic, GHC.Generic, Encode, Decode)

$(deriveJSON (defaultOptions
    { constructorTagModifier = over _head toLower, sumEncoding = ObjectWithSingleField }) ''StorageEntryType)

data StorageEntryMetadata = StorageEntryMetadata
    { entryName          :: !Text
    , entryModifier      :: !V12.StorageEntryModifier
    , entryType          :: !StorageEntryType
    , entryFallback      :: !HexString
    , entryDocumentation :: ![Text]
    } deriving (Eq, Show, Generic, GHC.Generic, Encode, Decode)

$(deriveJSON (defaultOptions
    { fieldLabelModifier = over _head toLower . drop 5 }) ''StorageEntryMetadata)

data StorageMetadata = StorageMetadata
    { storagePrefix :: !Text
    , storageItems  :: ![StorageEntryMetadata]
    } deriving (Eq, Show, Generic, GHC.Generic, Encode, Decode)

$(deriveJSON (defaultOptions
    { fieldLabelModifier = over _head toLower . drop 7 }) ''StorageMetadata)

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
