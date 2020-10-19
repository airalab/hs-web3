{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

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

import           Codec.Scale                           (Decode, Encode, Generic)
import qualified GHC.Generics                          as GHC (Generic)
import           Network.Polkadot.Metadata.MagicNumber (MagicNumber)
import           Network.Polkadot.Metadata.V10         (MetadataV10)
import           Network.Polkadot.Metadata.V11         (MetadataV11)
import           Network.Polkadot.Metadata.V12         (MetadataV12)
import           Network.Polkadot.Metadata.V9          (MetadataV9)

-- | The versioned runtime metadata as a decoded structure.
data Metadata = MetadataVersioned MagicNumber MetadataAll
  deriving (Eq, Show, Generic, GHC.Generic, Decode, Encode)

-- | All supported metadata versions as enum.
--
-- It could have troubles of decoding for metadata V9 because of hack:
-- https://github.com/polkadot-js/api/commit/a9211690be6b68ad6c6dad7852f1665cadcfa5b2
data MetadataAll
  = V0 | V1 | V2 | V3 | V4 | V5 | V6 | V7 | V8  -- Not used
  | V9 MetadataV9
  | V10 MetadataV10
  | V11 MetadataV11
  | V12 MetadataV12
  deriving (Eq, Show, Generic, GHC.Generic, Decode, Encode)

isV9 :: Metadata -> Bool
isV9 (MetadataVersioned _ (V9 _)) = True
isV9 _                            = False

isV10 :: Metadata -> Bool
isV10 (MetadataVersioned _ (V10 _)) = True
isV10 _                             = False

isV11 :: Metadata -> Bool
isV11 (MetadataVersioned _ (V11 _)) = True
isV11 _                             = False

isV12 :: Metadata -> Bool
isV12 (MetadataVersioned _ (V12 _)) = True
isV12 _                             = False

isLatest :: Metadata -> Bool
isLatest = isV12
