{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :  Network.Polkadot.Metadata.V11
-- Copyright   :  Aleksandr Krupenkin 2016-2020
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Metadata version 11 definitions.
--

module Network.Polkadot.Metadata.V11 where

import           Codec.Scale                     (Decode, Encode, Generic)
import           Data.Aeson                      (Options (fieldLabelModifier),
                                                  defaultOptions)
import           Data.Aeson.TH                   (deriveJSON)
import           Data.ByteArray.HexString        (HexString)
import           Data.Char                       (toLower)
import           Data.Text                       (Text)
import           Data.Word                       (Word8)
import qualified GHC.Generics                    as GHC (Generic)
import           Lens.Micro                      (over, _head)

import           Network.Polkadot.Metadata.Types (Type)
import qualified Network.Polkadot.Metadata.V10   as V10

type StorageEntryModifier = V10.StorageEntryModifier
type FunctionMetadata = V10.FunctionMetadata
type EventMetadata = V10.EventMetadata
type ModuleConstantMetadata = V10.ModuleConstantMetadata
type ErrorMetadata = V10.ErrorMetadata

data StorageHasher
    = Blake2_128
    | Blake2_256
    | Blake2_128Concat
    | Twox128
    | Twox256
    | Twox64Concat
    | Identity
    deriving (Eq, Show, Generic, GHC.Generic, Encode, Decode)

$(deriveJSON defaultOptions ''StorageHasher)

data MapType = MapType
    { mapHasher :: !StorageHasher
    , mapKey    :: !Type
    , mapValue  :: !Type
    , mapLinked :: !Bool
    } deriving (Eq, Show, Generic, GHC.Generic, Encode, Decode)

$(deriveJSON (defaultOptions
    { fieldLabelModifier = over _head toLower . drop 3 }) ''MapType)

data DoubleMapType = DoubleMapType
    { doubleMapHasher     :: !StorageHasher
    , doubleMapKey1       :: !Type
    , doubleMapKey2       :: !Type
    , doubleMapValue      :: !Type
    , doubleMapKey2Hasher :: !StorageHasher
    } deriving (Eq, Show, Generic, GHC.Generic, Encode, Decode)

$(deriveJSON (defaultOptions
    { fieldLabelModifier = over _head toLower . drop 9 }) ''DoubleMapType)

data StorageEntryType
    = Plain !Type
    | Map !MapType
    | DoubleMap !DoubleMapType
    deriving (Eq, Show, Generic, GHC.Generic, Encode, Decode)

$(deriveJSON defaultOptions ''StorageEntryType)

data StorageEntryMetadata = StorageEntryMetadata
    { entryName          :: !Text
    , entryModifier      :: !StorageEntryModifier
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
    } deriving (Eq, Show, Generic, GHC.Generic, Encode, Decode)

$(deriveJSON (defaultOptions
    { fieldLabelModifier = over _head toLower . drop 6 }) ''ModuleMetadata)

data ExtrinsicMetadata = ExtrinsicMetadata
    { extrinsicVersion          :: !Word8
    , extrinsicSignedExtensions :: ![Text]
    } deriving (Eq, Show, Generic, GHC.Generic, Encode, Decode)

$(deriveJSON (defaultOptions
    { fieldLabelModifier = over _head toLower . drop 9 }) ''ExtrinsicMetadata)

data Metadata = Metadata
    { modules    :: ![ModuleMetadata]
    , extrinsics :: ![ExtrinsicMetadata]
    } deriving (Eq, Show, Generic, GHC.Generic, Encode, Decode)

$(deriveJSON defaultOptions ''Metadata)
