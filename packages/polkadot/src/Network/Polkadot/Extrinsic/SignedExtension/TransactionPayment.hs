{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeFamilies   #-}

-- |
-- Module      :  Network.Polkadot.Extrinsic.SignedExtension.TransactionPayment
-- Copyright   :  Aleksandr Krupenkin 2016-2024
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- A pallet-transaction-payment signed extension.
--

module Network.Polkadot.Extrinsic.SignedExtension.TransactionPayment where

import           Codec.Scale                                (Compact, Decode,
                                                             Encode, Generic)
import qualified GHC.Generics                               as GHC (Generic)

import           Network.Polkadot.Extrinsic.SignedExtension (SignedExtension (..))
import           Network.Polkadot.Primitives                (Balance)

-- | Require the transactor pay for themselves and maybe include a tip to
-- gain additional priority in the queue.
newtype ChargeTransactionPayment = ChargeTransactionPayment (Compact Balance)
    deriving (Eq, Show, Generic, GHC.Generic, Encode, Decode)

instance SignedExtension ChargeTransactionPayment where
    type AdditionalSigned ChargeTransactionPayment = ()
    additional_signed _ = return ()
