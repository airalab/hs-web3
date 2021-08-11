{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :  Network.Polkadot.Metadata.V10
-- Copyright   :  Aleksandr Krupenkin 2016-2021
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Metadata version 10 definitions.
--

module Network.Polkadot.Metadata.V10 where

import           Codec.Scale                    (Decode, Encode, Generic)
import           Data.Aeson                     (Options (constructorTagModifier, fieldLabelModifier, sumEncoding),
                                                 SumEncoding (ObjectWithSingleField),
                                                 defaultOptions)
import           Data.Aeson.TH                  (deriveJSON)
import           Data.ByteArray.HexString       (HexString)
import           Data.Char                      (toLower)
import           Data.Text                      (Text)
import qualified GHC.Generics                   as GHC (Generic)
import           Lens.Micro                     (_head, over)

import           Network.Polkadot.Metadata.Type (Type)
import qualified Network.Polkadot.Metadata.V9   as V9

type StorageEntryModifier = V9.StorageEntryModifier
type FunctionMetadata = V9.FunctionMetadata
type EventMetadata = V9.EventMetadata
type ModuleConstantMetadata = V9.ModuleConstantMetadata
type ErrorMetadata = V9.ErrorMetadata

data StorageHasher
    = Blake2_128
    | Blake2_256
    | Blake2_128Concat
    | Twox128
    | Twox256
    | Twox64Concat
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

$(deriveJSON (defaultOptions
    { constructorTagModifier = over _head toLower, sumEncoding = ObjectWithSingleField }) ''StorageEntryType)

data StorageEntryMetadata = StorageEntryMetadata
    { entryName     :: !Text
    , entryModifier :: !StorageEntryModifier
    , entryType     :: !StorageEntryType
    , entryFallback :: !HexString
    , entryDocs     :: ![Text]
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

data Metadata = Metadata
    { modules :: ![ModuleMetadata]
    } deriving (Eq, Show, Generic, GHC.Generic, Encode, Decode)

$(deriveJSON defaultOptions ''Metadata)
