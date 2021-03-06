{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :  Network.Polkadot.Metadata
-- Copyright   :  Aleksandr Krupenkin 2016-2020
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Node runtime type information.
--

module Network.Polkadot.Metadata where

import           Codec.Scale                              (Decode, Encode,
                                                           Generic)
import           Data.Aeson                               (Options (sumEncoding),
                                                           SumEncoding (ObjectWithSingleField),
                                                           defaultOptions)
import           Data.Aeson.TH                            (deriveJSON)
import           Data.Set                                 (Set)
import qualified GHC.Generics                             as GHC (Generic)

import           Network.Polkadot.Metadata.MagicNumber    (MagicNumber (..))
import           Network.Polkadot.Metadata.Type           (Type)
import           Network.Polkadot.Metadata.Type.Discovery (runDiscovery)
import qualified Network.Polkadot.Metadata.V10            as V10 (Metadata (Metadata),
                                                                  moduleName)
import qualified Network.Polkadot.Metadata.V11            as V11 (Metadata (Metadata),
                                                                  moduleName)
import qualified Network.Polkadot.Metadata.V12            as V12 (Metadata (Metadata),
                                                                  moduleName)
import qualified Network.Polkadot.Metadata.V9             as V9 (Metadata (Metadata),
                                                                 moduleName)

-- | All supported metadata versions as enum.
--
-- It could have troubles of decoding for metadata V9 because of hack:
-- https://github.com/polkadot-js/api/commit/a9211690be6b68ad6c6dad7852f1665cadcfa5b2
data MetadataVersioned
  = V0 | V1 | V2 | V3 | V4 | V5 | V6 | V7 | V8  -- Not defined
  | V9 V9.Metadata
  | V10 V10.Metadata
  | V11 V11.Metadata
  | V12 V12.Metadata
  deriving (Eq, Show, Generic, GHC.Generic, Decode, Encode)

$(deriveJSON (defaultOptions { sumEncoding = ObjectWithSingleField }) ''MetadataVersioned)

-- | The versioned runtime metadata as a decoded structure.
data Metadata = Metadata
    { magicNumber :: MagicNumber
    , metadata    :: MetadataVersioned
    } deriving (Eq, Show, Generic, GHC.Generic, Decode, Encode)

$(deriveJSON defaultOptions ''Metadata)

isV9 :: Metadata -> Bool
isV9 (Metadata _ (V9 _)) = True
isV9 _                   = False

isV10 :: Metadata -> Bool
isV10 (Metadata _ (V10 _)) = True
isV10 _                    = False

isV11 :: Metadata -> Bool
isV11 (Metadata _ (V11 _)) = True
isV11 _                    = False

isV12 :: Metadata -> Bool
isV12 (Metadata _ (V12 _)) = True
isV12 _                    = False

isLatest :: Metadata -> Bool
isLatest = isV12

toLatest :: Metadata -> V12.Metadata
toLatest (Metadata _ (V12 m)) = m
toLatest _                    = undefined

metadataTypes :: Metadata -> (Metadata, Set Type)
metadataTypes (Metadata _ (V9 (V9.Metadata modules))) =
    let (modules', types) = runDiscovery V9.moduleName modules
    in (Metadata MagicNumber (V9 (V9.Metadata modules')), types)

metadataTypes (Metadata _ (V10 (V10.Metadata modules))) =
    let (modules', types) = runDiscovery V10.moduleName modules
    in (Metadata MagicNumber (V10 (V10.Metadata modules')), types)

{- XXX: OOM compilation on my laptop
metadataTypes (Metadata _ (V11 (V11.Metadata modules extrinsics))) =
    let (modules', types) = runDiscovery V11.moduleName modules
    in (Metadata MagicNumber (V11 (V11.Metadata modules' extrinsics)), types)
-}

metadataTypes (Metadata _ (V12 (V12.Metadata modules extrinsics))) =
    let (modules', types) = runDiscovery V12.moduleName modules
    in (Metadata MagicNumber (V12 (V12.Metadata modules' extrinsics)), types)

metadataTypes m = (m, mempty)
