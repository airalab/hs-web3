{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      :  Network.Polkadot.Primitives
-- Copyright   :  Aleksandr Krupenkin 2016-2020
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- Polkadot primitive data types.
--

module Network.Polkadot.Primitives
   ( Balance
   , BlockNumber
   , Moment
   , AccountIndex
   , AccountId
   , AccountData(..)
   , AccountInfo(..)
   , H256
   , Word128
   ) where

import           Codec.Scale  (Decode, Encode)
import           Data.BigNum  (H256, Word128)
import           Data.Word    (Word32, Word64)
import           Generics.SOP (Generic)
import qualified GHC.Generics as GHC (Generic)

-- | The user account balance, 'u128' type.
type Balance = Word128

-- | Block numbers is up to 2^32.
type BlockNumber = Word32

-- | Time moment is 64bit.
type Moment = Word64

-- | Accounts in system is up to 2^32.
type AccountIndex = Word32

-- | The user account identifier type for the runtime.
type AccountId = H256

-- | Account balances.
data AccountData = AccountData
  { balanceFree     :: Balance
  , balanceReserved :: Balance
  , miscFrozen      :: Balance
  , feeFrozen       :: Balance
  } deriving (Eq, Ord, Show, GHC.Generic, Generic, Encode, Decode)

-- | General account information.
data AccountInfo = AccountInfo
  { accountNonce    :: AccountIndex
  , accountRefcount :: Word32
  , accountData     :: AccountData
  } deriving (Eq, Ord, Show, GHC.Generic, Generic, Encode, Decode)
