{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE TypeFamilies   #-}

-- |
-- Module      :  Network.Polkadot.Extrinsic.Signature
-- Copyright   :  Aleksandr Krupenkin 2016-2021
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Extrinsic payload signing and signature types.
--

module Network.Polkadot.Extrinsic.Signature where

import           Codec.Scale                    (encode)
import           Codec.Scale.Class              (Decode (..), Encode (..))
import           Data.Word                      (Word32)
import           Generics.SOP                   (Generic)
import qualified GHC.Generics                   as GHC (Generic)
import           Network.Polkadot.Extrinsic.Era (Era)
import           Network.Polkadot.Primitives    (Balance, Hash, Index,
                                                 MultiSignature)

-- A container for the Signature associated with a specific Extrinsic.
{-
data Signature where
    Sign :: (Signer a, Encode b) => a -> Payload b -> Signature
    Raw :: (Decode s, Decode a) => s -> Payload a -> Signature

instance Encode Signature where
    put s = case s of
        Sign s p -> put (sign s $ encode p) >> put p
        Raw s p  -> put s >> put p

instance Decode Signature where
    get = Raw <$> get <*> get
-}

-- | Extrinsic payload data.
data Payload a = Payload
  { blockHash          :: Hash
  -- ^ The block 'Hash' the signature applies to (mortal/immortal).
  , era                :: Era
  -- ^ The 'Era' of extrinsic.
  , genesisHash        :: Hash
  -- ^ The genesis 'Hash' the signature applies to (mortal/immortal).
  , method             :: a
  -- ^ The encoded method contained in the payload.
  , nonce              :: Index
  -- ^ Transaction number that can only be used once.
  , specVersion        :: Word32
  -- ^ The specVersion for this signature.
  , tip                :: Balance
  -- ^ Additional incentive for validator to include this trasaction into block.
  , transactionVersion :: Word32
  -- ^ The transactionVersion for this signature.
  } deriving (Eq, Ord, Show, GHC.Generic, Generic, Encode, Decode)
