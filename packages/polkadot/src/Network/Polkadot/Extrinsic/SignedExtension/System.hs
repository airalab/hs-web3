{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeFamilies   #-}

-- |
-- Module      :  Network.Polkadot.Extrinsic.SignedExtension.System
-- Copyright   :  Aleksandr Krupenkin 2016-2021
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- A frame system signed extensions.
--

module Network.Polkadot.Extrinsic.SignedExtension.System where

import           Codec.Scale                                (Compact, Decode,
                                                             Encode, Generic)
import           Data.BigNum                                (h256)
import           Data.Maybe                                 (fromJust)
import           Data.Word                                  (Word32)
import qualified GHC.Generics                               as GHC (Generic)

import           Network.Polkadot.Extrinsic.Era             (Era, birth)
import           Network.Polkadot.Extrinsic.SignedExtension (SignedExtension (..))
import           Network.Polkadot.Primitives                (Hash, Index)
import           Network.Polkadot.Rpc.Chain                 (getBlockHash,
                                                             getHeader)
import           Network.Polkadot.Rpc.State                 (getRuntimeVersion)
import           Network.Polkadot.Rpc.Types                 (RuntimeVersion (..),
                                                             headerNumber,
                                                             unBlockNumber)

-- | Ensure the runtime version registered in the transaction is the same as at present.
data CheckSpecVersion = CheckSpecVersion
    deriving (Eq, Show, Generic, GHC.Generic, Encode, Decode)

instance SignedExtension CheckSpecVersion where
    type AdditionalSigned CheckSpecVersion = Word32
    additional_signed _ = runtimeSpecVersion <$> getRuntimeVersion Nothing

data CheckTxVersion = CheckTxVersion
    deriving (Eq, Show, Generic, GHC.Generic, Encode, Decode)

instance SignedExtension CheckTxVersion where
    type AdditionalSigned CheckTxVersion = Word32
    additional_signed _ = runtimeTransactionVersion <$> getRuntimeVersion Nothing

data CheckGenesis = CheckGenesis
    deriving (Eq, Show, Generic, GHC.Generic, Encode, Decode)

instance SignedExtension CheckGenesis where
    type AdditionalSigned CheckGenesis = Hash
    additional_signed _ = do
        -- chain must have genesis block, fromJust is safe
        (fromJust . (h256 =<<)) <$> getBlockHash (Just 0)

newtype CheckEra = CheckEra Era
    deriving (Eq, Show, Generic, GHC.Generic, Encode, Decode)

instance SignedExtension CheckEra where
    type AdditionalSigned CheckEra = Hash
    additional_signed (CheckEra era) = do
        -- chain must have top header, fromJust is safe here
        current <- (unBlockNumber . headerNumber . fromJust) <$> getHeader Nothing
        (fromJust . (h256 =<<)) <$> getBlockHash (Just $ birth era current)

newtype CheckNonce = CheckNonce (Compact Index)
    deriving (Eq, Show, Generic, GHC.Generic, Encode, Decode)

instance SignedExtension CheckNonce where
    type AdditionalSigned CheckNonce = ()
    additional_signed _ = return ()

data CheckWeight = CheckWeight
    deriving (Eq, Show, Generic, GHC.Generic, Encode, Decode)

instance SignedExtension CheckWeight where
    type AdditionalSigned CheckWeight = ()
    additional_signed _ = return ()
