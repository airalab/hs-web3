module Crypto.Ethereum where

import           Crypto.Hash                (Keccak_256 (..), hashWith)
import           Crypto.Secp256k1           (CompactRecSig, SecKey,
                                             exportCompactRecSig,
                                             importCompactRecSig, msg, recover,
                                             signRecMsg)
import           Data.ByteArray             (ByteArrayAccess, convert)

import           Data.Solidity.Prim.Address (Address, fromPubKey)

-- | Sign message with Ethereum private key
ecsign :: ByteArrayAccess message
       => message
       -- ^ Message content
       -> SecKey
       -- ^ Private key
       -> Maybe CompactRecSig
       -- ^ Signature
ecsign message privateKey = do
    msgHash <- msg $ convert $ hashWith Keccak_256 message
    return . exportCompactRecSig $ signRecMsg privateKey msgHash

-- | Recover message signer Ethereum address
ecrecover :: ByteArrayAccess message
          => message
          -- ^ Message content
          -> CompactRecSig
          -- ^ Signature
          -> Maybe Address
          -- ^ Message signer address
ecrecover message sig = do
    msgHash <- msg $ convert $ hashWith Keccak_256 message
    sig' <- importCompactRecSig sig
    fromPubKey <$> recover sig' msgHash
