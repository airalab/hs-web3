{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Crypto.Ecdsa.Signature
-- Copyright   :  Alexander Krupenkin 2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Recoverable Ethereum signature support.
--

module Crypto.Ethereum.Signature
    (
      hashMessage
    , signMessage
    , signTransaction
    ) where

import           Crypto.Hash             (Digest, Keccak_256 (..), hashWith)
import           Crypto.PubKey.ECC.ECDSA (PrivateKey (..))
import           Data.ByteArray          (ByteArray, ByteArrayAccess, convert)
import qualified Data.ByteArray          as BA (length)
import           Data.ByteString.Builder (intDec, toLazyByteString)
import qualified Data.ByteString.Lazy    as LBS (toStrict)
import           Data.Monoid             ((<>))
import           Data.Word               (Word8)

import           Crypto.Ecdsa.Signature  (pack, sign)

-- | Make Ethereum standard signature.
--
-- The message is before enveloped as follows:
-- "\x19Ethereum Signed Message:\n" + message.length + message
--
-- /WARNING:/ Vulnerable to timing attacks.
signMessage :: (ByteArrayAccess message, ByteArray rsv)
            => PrivateKey
            -> message
            -> rsv
{-# INLINE signMessage #-}
signMessage pk = pack . sign pk . hashMessage

-- | Ethereum standard hashed message.
--
-- The data will be UTF-8 HEX decoded and enveloped as follows:
-- "\x19Ethereum Signed Message:\n" + message.length + message and hashed using keccak256.
hashMessage :: ByteArrayAccess message => message -> Digest Keccak_256
hashMessage msg = hashWith Keccak_256 prefixed
  where
    len = LBS.toStrict . toLazyByteString . intDec . BA.length
    prefixed = "\x19" <> "Ethereum Signed Message:\n" <> len msg <> convert msg

-- | Sign Ethereum transaction.
--
-- /WARNING:/ Vulnerable to timing attacks.
signTransaction :: ByteArray ba
                => (Maybe (Integer, Integer, Word8) -> ba)
                -- ^ Two way transaction packer (unsigned and signed)
                -> PrivateKey
                -- ^ Private key
                -> ba
                -- ^ Encoded transaction
signTransaction encode key = encode $ Just signed
  where
    unsigned = encode Nothing
    signed = sign key (hashWith Keccak_256 unsigned)
