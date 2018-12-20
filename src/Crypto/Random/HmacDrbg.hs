-- |
-- Module      :  Crypto.Random.HmacDrbg
-- Copyright   :  Alexander Krupenkin 2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- NIST standardized number-theoretically secure random number generator.
-- https://csrc.nist.gov/csrc/media/events/random-number-generation-workshop-2004/documents/hashblockcipherdrbg.pdf
--
-- XXX: This algorithm requires reseed after 2^48 iterations.
--
-- > Inspired by https://github.com/TomMD/DRBG and https://github.com/indutny/hmac-drbg.
--

module Crypto.Random.HmacDrbg
    (
      HmacDrbg
    , initialize
    ) where


import           Crypto.Hash     (HashAlgorithm, digestFromByteString,
                                  hashDigestSize)
import           Crypto.MAC.HMAC (HMAC (..), hmac)
import           Crypto.Random   (DRG (..))
import           Data.ByteArray  (ByteArray, convert, singleton)
import qualified Data.ByteArray  as BA (null, take)
import qualified Data.ByteString as B (replicate)
import           Data.Maybe      (fromJust)
import           Data.Monoid     ((<>))
import           Data.Word       (Word8)

-- | HMAC Deterministic Random Bytes Generator.
newtype HmacDrbg a = HmacDrbg (HMAC a, HMAC a)
    deriving Eq

instance HashAlgorithm a => DRG (HmacDrbg a) where
    randomBytesGenerate = generate

update :: (ByteArray bin, HashAlgorithm a)
       => bin
       -> HmacDrbg a
       -> HmacDrbg a
update input | BA.null input = go 0x00
             | otherwise = go 0x01 . go 0x00
  where
    go :: HashAlgorithm a => Word8 -> HmacDrbg a -> HmacDrbg a
    go c (HmacDrbg (k, v)) = let k' = hmac k (convert v <> singleton c <> input)
                                 v' = hmac k' v
                             in HmacDrbg (k', v')

-- | Initialize HMAC-DRBG by seed.
initialize :: (ByteArray seed, HashAlgorithm a)
           => seed
           -> HmacDrbg a
initialize = flip update $ HmacDrbg (hmac0 undefined 0x00, hmac0 undefined 0x01)
  where
    hmac0 :: HashAlgorithm a => a -> Word8 -> HMAC a
    hmac0 a = HMAC . fromJust . digestFromByteString . B.replicate (hashDigestSize a)

generate :: (HashAlgorithm a, ByteArray output) => Int -> HmacDrbg a -> (output, HmacDrbg a)
generate reqBytes (HmacDrbg (k, v)) = (output, HmacDrbg (k, vFinal))
  where
    getV (u, rest) = let v' = hmac k u in (v', rest <> convert v')
    (vFinal, output) = BA.take reqBytes <$> iterate getV (v, mempty) !! reqBytes
