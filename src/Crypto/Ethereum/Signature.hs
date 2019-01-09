{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Crypto.Ethereum
-- Copyright   :  Alexander Krupenkin 2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Recoverable ECC signature support.
--

module Crypto.Ethereum.Signature
    (
      hashMessage
    , signMessage
    , signTransaction
    , pack
    , unpack
    ) where

import           Control.Monad               (when)
import           Crypto.Hash                 (Digest, Keccak_256 (..), SHA256,
                                              hashWith)
import           Crypto.Number.Generate      (generateBetween)
import           Crypto.Number.ModArithmetic (inverse)
import           Crypto.Number.Serialize     (i2osp, os2ip)
import           Crypto.PubKey.ECC.ECDSA     (PrivateKey (..))
import           Crypto.PubKey.ECC.Prim      (pointMul)
import           Crypto.PubKey.ECC.Types     (CurveCommon (ecc_g, ecc_n),
                                              Point (..), common_curve)
import           Crypto.Random               (MonadRandom, withDRG)
import           Crypto.Random.HmacDrbg      (HmacDrbg, initialize)
import           Data.Bits                   (xor, (.|.))
import           Data.ByteArray              (ByteArray, ByteArrayAccess,
                                              convert, singleton, takeView,
                                              view)
import qualified Data.ByteArray              as BA (length, unpack)
import           Data.ByteString.Builder     (intDec, toLazyByteString)
import qualified Data.ByteString.Lazy        as LBS (toStrict)
import           Data.Monoid                 ((<>))
import           Data.Word                   (Word8)

import           Crypto.Ethereum.Utils       (exportKey)
import           Data.ByteArray.HexString    (HexString)
import           Data.Solidity.Prim.Address  (Address)

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

-- | Sign arbitrary data by given private key.
--
-- /WARNING:/ Vulnerable to timing attacks.
sign :: ByteArrayAccess bin
     => PrivateKey
     -> bin
     -> (Integer, Integer, Word8)
sign pk bin = fst $ withDRG hmac_drbg $ ecsign pk (os2ip truncated)
  where
    hmac_drbg :: HmacDrbg SHA256
    hmac_drbg = initialize $ exportKey pk <> truncated
    truncated = convert $ takeView bin 32 :: HexString

ecsign :: MonadRandom m
       => PrivateKey
       -> Integer
       -> m (Integer, Integer, Word8)
ecsign pk@(PrivateKey curve d) z = do
    k <- generateBetween 0 (n - 1)
    case trySign k of
        Nothing  -> ecsign pk z
        Just rsv -> return rsv
  where
    n = ecc_n (common_curve curve)
    g = ecc_g (common_curve curve)
    recoveryParam x y r = fromIntegral $
        fromEnum (odd y) .|. if x /= r then 2 else 0
    trySign k = do
        (kpX, kpY) <- case pointMul curve k g of
            PointO    -> Nothing
            Point x y -> return (x, y)
        let r = kpX `mod` n
        kInv <- inverse k n
        let s = kInv * (z + r * d) `mod` n
        when (r == 0 || s == 0) Nothing
        -- Recovery param
        let v = recoveryParam kpX kpY r
        -- Use complement of s if it > n / 2
        let (s', v') | s > n `div` 2 = (n - s, v `xor` 1)
                     | otherwise = (s, v)
        return $ (r, s', v' + 27)

-- | Unpack recoverable signature from byte array.
--
-- Input array should have 65 byte length.
unpack :: ByteArrayAccess rsv => rsv -> (Integer, Integer, Word8)
unpack vrs = (r, s, v)
  where
    r = os2ip (view vrs 1 33)
    s = os2ip (view vrs 33 65)
    v = head (BA.unpack vrs)

-- | Pack recoverable signature as byte array (65 byte length).
pack :: ByteArray rsv => (Integer, Integer, Word8) -> rsv
pack (r, s, v) = i2osp r <> i2osp s <> singleton v
