{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Crypto.Ecdsa.Signature
-- Copyright   :  Aleksandr Krupenkin 2016-2020
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Recoverable ECC signature support.
--

module Crypto.Ecdsa.Signature
    (
      sign
    , pack
    , unpack
    ) where

import           Control.Monad               (when)
import           Crypto.Hash                 (SHA256)
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
import           Data.ByteArray              (ByteArray, ByteArrayAccess, Bytes,
                                              convert, singleton, takeView,
                                              view)
import qualified Data.ByteArray              as BA (unpack)
import           Data.Word                   (Word8)

import           Crypto.Ecdsa.Utils          (exportKey)

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
    truncated = convert $ takeView bin 32 :: Bytes

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
        return (r, s', v' + 27)

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
