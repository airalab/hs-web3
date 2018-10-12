module Crypto.Ethereum where

import           Crypto.Hash                (Keccak_256 (..), hashWith)
import           Crypto.Secp256k1           (CompactRecSig, Msg, SecKey,
                                             exportCompactRecSig,
                                             importCompactRecSig, msg, recover,
                                             signRecMsg)
import           Data.ByteArray             (ByteArrayAccess, convert)
import           Data.Maybe                 (fromJust)

import           Data.Solidity.Prim.Address (Address, fromPubKey)

-- | Keccak 256 hash of argument
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
