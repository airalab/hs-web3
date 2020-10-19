{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

-- |
-- Module      :  Network.Polkadot.Metadata.V9
-- Copyright   :  Aleksandr Krupenkin 2016-2020
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Metadata V9 data type.
--

module Network.Polkadot.Metadata.V9 where

import           Codec.Scale     (Decode, Encode, Generic)
import           Data.ByteString (ByteString)
import           Data.Text       (Text)
import qualified GHC.Generics    as GHC (Generic)

-- TODO
type Type = Text

data MetadataV9 = MetadataV9 [ModuleMetadataV9]
    deriving (Eq, Show, Generic, GHC.Generic, Encode, Decode)

data ModuleMetadataV9 = ModuleMetadataV9
    { moduleName      :: Text
    , moduleStorage   :: Maybe StorageMetadataV9
    , moduleCalls     :: Maybe [FunctionMetadataV9]
    , moduleEvents    :: Maybe [EventMetadataV9]
    , moduleConstants :: [ModuleConstantMetadataV9]
    , moduleErrors    :: [ErrorMetadataV9]
    } deriving (Eq, Show, Generic, GHC.Generic, Encode, Decode)

data StorageMetadataV9 = StorageMetadataV9
    { storagePrefix :: Text
    , storageItems  :: [StorageEntryMetadataV9]
    } deriving (Eq, Show, Generic, GHC.Generic, Encode, Decode)

data StorageEntryMetadataV9 = StorageEntryMetadataV9
    { entryName          :: Text
    , entryModifier      :: StorageEntryModifierV9
    , entryType          :: StorageEntryTypeV9
    , entryFallback      :: ByteString
    , entryDocumentation :: [Text]
    } deriving (Eq, Show, Generic, GHC.Generic, Encode, Decode)

data StorageEntryModifierV9 = OptionalModifier
                            | DefaultModifier
                            | RequiredModifier
    deriving (Eq, Show, Generic, GHC.Generic, Encode, Decode)

data StorageEntryTypeV9 = PlainType Type
                        | MapType MapTypeV9
                        | DoubleMapType DoubleMapTypeV9
    deriving (Eq, Show, Generic, GHC.Generic, Encode, Decode)

data MapTypeV9 = MapTypeV9
    { mapHasher :: StorageHasherV9
    , mapKey    :: Type
    , mapValue  :: Type
    , mapLinked :: Bool
    } deriving (Eq, Show, Generic, GHC.Generic, Encode, Decode)

data DoubleMapTypeV9 = DoubleMapTypeV9
    { doubleMapHasher     :: StorageHasherV9
    , doubleMapKey1       :: Type
    , doubleMapKey2       :: Type
    , doubleMapValue      :: Type
    , doubleMapKey2Hasher :: StorageHasherV9
    } deriving (Eq, Show, Generic, GHC.Generic, Encode, Decode)

data StorageHasherV9 = Blake2128 | Blake2256 | Twox128 | Twox256 | Twox64Concat
    deriving (Eq, Show, Generic, GHC.Generic, Encode, Decode)

data FunctionMetadataV9 = FunctionMetadataV9
    { functionName          :: Text
    , functionArgs          :: [FunctionArgumentMetadataV9]
    , functionDocumentation :: [Text]
    } deriving (Eq, Show, Generic, GHC.Generic, Encode, Decode)

data FunctionArgumentMetadataV9 = FunctionArgumentMetadataV9
    { argumentName :: Text
    , argumentType :: Type
    } deriving (Eq, Show, Generic, GHC.Generic, Encode, Decode)

data EventMetadataV9 = EventMetadataV9
    { eventName          :: Text
    , eventArgs          :: [Type]
    , eventDocumentation :: [Text]
    } deriving (Eq, Show, Generic, GHC.Generic, Encode, Decode)

data ModuleConstantMetadataV9 = ModuleConstantMetadataV9
    { constantName          :: Text
    , constantType          :: Type
    , constantValue         :: ByteString
    , constantDocumentation :: [Text]
    } deriving (Eq, Show, Generic, GHC.Generic, Encode, Decode)

data ErrorMetadataV9 = ErrorMetadataV9
    { errorName          :: Text
    , errorDocumentation :: [Text]
    } deriving (Eq, Show, Generic, GHC.Generic, Encode, Decode)
