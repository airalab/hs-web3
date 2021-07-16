{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      :  Network.Polkadot.Primitives
-- Copyright   :  Aleksandr Krupenkin 2016-2021
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- Polkadot primitive data types.
--

module Network.Polkadot.Primitives where

import           Codec.Scale              (Decode, Encode)
import           Codec.Scale.Compact      (Compact)
import           Data.BigNum              (H160, H256, H512, Word128)
import           Data.ByteArray.HexString (HexString)
import           Data.Word                (Word32, Word64, Word8)
import           Generics.SOP             (Generic)
import qualified GHC.Generics             as GHC (Generic)

-- | The user account balance, 'u128' type.
type Balance = Word128

-- | Block numbers is up to 2^32.
type BlockNumber = Word32

-- | Time moment is 64bit.
type Moment = Word64

-- | Transaction nonce value.
type Index = Word32

-- | The type for looking up accounts. We don't expect more than 4 billion of them.
type AccountIndex = Word32

-- | The user account identifier type for the runtime.
type AccountId = H256

-- | The type for runtime hashes.
type Hash = H256

-- | Account balances.
data AccountData = AccountData
  { balanceFree     :: !Balance
  , balanceReserved :: !Balance
  , miscFrozen      :: !Balance
  , feeFrozen       :: !Balance
  } deriving (Eq, Ord, Show, GHC.Generic, Generic, Encode, Decode)

-- | General account information.
data AccountInfo = AccountInfo
  { accountNonce    :: !Index
  , accountRefcount :: !Word32
  , accountData     :: !AccountData
  } deriving (Eq, Ord, Show, GHC.Generic, Generic, Encode, Decode)

-- | Multiple signatures support type.
data MultiSignature
  = Ed25519Signature !H512
  | Sr25519Signature !H512
  | EcdsaSignature !H512 !Word8
  deriving (Eq, Ord, Show, GHC.Generic, Generic, Encode, Decode)

-- | Cryptographic key for any known crypto algorithm.
data MultiSigner
    = Sr25519Signer
    -- ^ sr25519 crypto has no support yet
    | Ed25519Signer !H256
    -- ^ Ed25519 public key.
    | EcdsaSigner !H512
    -- ^ ECDSA public key.
  deriving (Eq, Ord, GHC.Generic, Generic, Encode, Decode)

-- | A multi-format address wrapper for on-chain accounts.
data MultiAddress
    = MaId !AccountId
    -- ^ It's an account ID (pubkey).
    | MaIndex !(Compact AccountIndex)
    -- ^ It's an account index.
    | MaRaw !HexString
    -- ^ It's some arbitrary raw bytes.
    | MaAddress32 !H256
    -- ^ It's a 32 byte representation.
    | MaAddress20 !H160
    -- ^ Its a 20 byte representation.
  deriving (Eq, Ord, GHC.Generic, Generic, Encode, Decode)
