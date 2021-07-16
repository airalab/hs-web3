{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Network.Polkadot.Account
-- Copyright   :  Aleksandr Krupenkin 2016-2021
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Polkadot account types.
--

module Network.Polkadot.Account where

import           Data.BigNum                 (h256)
import           Data.ByteArray              (convert)
import           Data.Digest.Blake2          (blake2_256)
import           Data.Maybe                  (fromJust)
import           Data.Text                   (Text)

import           Network.Polkadot.Primitives (MultiSigner (..))
import qualified Network.Polkadot.Primitives as P (AccountId)

-- | Some type that is able to be collapsed into an account ID.
--
-- It is not possible to recreate the original value from the account ID.
class IdentifyAccount a where
    -- | The account ID that this can be transformed into.
    type AccountId a

    -- | Transform into an account.
    into_account :: a -> AccountId a

instance IdentifyAccount MultiSigner where
    type AccountId MultiSigner = P.AccountId
    into_account (Ed25519Signer pub) = fromJust (h256 pub)
    into_account (EcdsaSigner pub)   = fromJust (h256 $ blake2_256 $ convert pub)
    into_account Sr25519Signer       = error "Sr25519 has no support yet"

instance Show MultiSigner where
    show = show . into_account

-- | Key that can be encoded to/from SS58.
--
-- See <https://github.com/paritytech/substrate/wiki/External-Address-Format-(SS58)#address-type>
-- for information on the codec.
class Ss58Codec a where
    -- | Some if the string is a properly encoded SS58Check address.
    from_ss58check :: Text -> Either String a

    -- | Return the ss58-check string for this key.
    to_ss58check :: a -> Text
