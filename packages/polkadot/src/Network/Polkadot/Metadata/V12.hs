{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

-- |
-- Module      :  Network.Polkadot.Metadata.V12
-- Copyright   :  Aleksandr Krupenkin 2016-2020
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Metadata V12 data type.
--

module Network.Polkadot.Metadata.V12 where

import           Codec.Scale  (Decode, Encode, Generic)
import qualified GHC.Generics as GHC (Generic)

data MetadataV12 = MetadataV12
    deriving (Eq, Show, Generic, GHC.Generic, Encode, Decode)
