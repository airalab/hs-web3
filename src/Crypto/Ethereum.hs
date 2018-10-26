-- |
-- Module      :  Crypto.Ethereum
-- Copyright   :  Alexander Krupenkin 2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- Ethereum ECDSA based on secp256k1 bindings.
--

module Crypto.Ethereum
    (
    -- * Ethereum ECDSA sign/recover
      hashMessage
    , ecsign
    , ecrecover

    -- * Re-export useful Secp256k1 functions
    , SecKey
    , derivePubKey
    )where

import           Crypto.Hash                (Keccak_256 (..), hashWith)
import           Crypto.Secp256k1           (CompactRecSig, Msg, SecKey,
                                             derivePubKey, exportCompactRecSig,
                                             importCompactRecSig, msg, recover,
                                             signRecMsg)
import           Data.ByteArray             (ByteArrayAccess, convert)
import           Data.Maybe                 (fromJust)

import           Data.Solidity.Prim.Address (Address, fromPubKey)

-- | SHA3 hash of argument
hashMessage :: ByteArrayAccess ba => ba -> Msg
hashMessage = fromJust . msg . convert . hashWith Keccak_256

-- | Sign message with Ethereum private key
ecsign :: ByteArrayAccess message
       => SecKey
       -- ^ Private key
       -> message
       -- ^ Message content
       -> CompactRecSig
       -- ^ Signature
ecsign key = exportCompactRecSig . signRecMsg key . hashMessage

-- | Recover message signer Ethereum address
ecrecover :: ByteArrayAccess message
          => CompactRecSig
          -- ^ Signature
          -> message
          -- ^ Message content
          -> Maybe Address
          -- ^ Message signer address
ecrecover sig message = do
    sig' <- importCompactRecSig sig
    fromPubKey <$> recover sig' (hashMessage message)
