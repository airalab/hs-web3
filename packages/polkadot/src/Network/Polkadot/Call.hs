{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

-- |
-- Module      :  Network.Polkadot.Extrinsic
-- Copyright   :  Aleksandr Krupenkin 2016-2021
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
--

module Network.Polkadot.Call where

import           Codec.Scale                 (Compact (..), Decode, Encode,
                                              Generic)
import qualified GHC.Generics                as GHC (Generic)

import           Network.Polkadot.Primitives (AccountId, Balance, MultiAddress)

data BalancesCall = Transfer MultiAddress (Compact Balance) | SetBalance
    deriving (Eq, Show, Generic, GHC.Generic, Encode, Decode)

data Call = System | Utility | Babe | Timestamp | Authorship | Indices | Balances BalancesCall
    deriving (Eq, Show, Generic, GHC.Generic, Encode, Decode)
