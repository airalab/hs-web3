-- |
-- Module      :  Network.Polkadot.Extrinsic.Era
-- Copyright   :  Aleksandr Krupenkin 2016-2024
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- An era to describe the longevity of a transaction.
--

module Network.Polkadot.Extrinsic.Era
    ( Era(..)
    , new_mortal_compact
    , birth
    , death
    ) where

import           Codec.Scale.Class (Decode (..), Encode (..))
import           Codec.Scale.Core  ()
import           Data.Bits         (shiftL, shiftR, (.|.))
import           Data.Word         (Word16, Word32, Word8, byteSwap16)

-- | The era for an extrinsic, indicating either a mortal or immortal extrinsic.
data Era
  = ImmortalEra
  -- ^ The ImmortalEra for an extrinsic.
  | MortalEra !Word32 !Word32
  -- ^ The MortalEra for an extrinsic, indicating period and phase.
  --
  -- Period and phase are encoded:
  -- - The period of validity from the block hash found in the signing material.
  -- - The phase in the period that this transaction's lifetime begins (and, importantly,
  -- implies which block hash is included in the signature material). If the `period` is
  -- greater than 1 << 12, then it will be a factor of the times greater than 1 << 12 that
  -- `period` is.
  deriving (Eq, Ord, Show)

instance Decode Era where
    get = do
        first <- get
        case first :: Word8 of
            0 -> return ImmortalEra
            _ -> decodeMortal first <$> get
      where
        decodeMortal :: Word8 -> Word8 -> Era
        decodeMortal first second =
            let first' = fromIntegral first
                second' = fromIntegral second
             in new_mortal_compact (first' + second' `shiftL` 8)

instance Encode Era where
    put ImmortalEra = put (0 :: Word8)
    put (MortalEra period' phase') = put encoded
      where
        encoded :: Word16
        encoded = first .|. second
        first = (1 `max` (trailing_zeros period - 1)) `min` 15
        second = (phase `div` quantizeFactor) `shiftL` 4
        quantizeFactor = max (period `shiftR` 12) 1
        period = fromIntegral period'
        phase = fromIntegral phase'

-- | Create a mortal 'Era' type from two bytes of data.
new_mortal_compact :: Word16 -> Era
new_mortal_compact raw = MortalEra period phase
  where
    era = fromIntegral $ byteSwap16 raw

    period = 2 `shiftL` (fromIntegral (era `rem` 16))
    quantizeFactor = max (period `shiftR` 12) 1
    phase = (era `shiftR` 4) * quantizeFactor

trailing_zeros :: Integral a => a -> a
trailing_zeros = foldl zero 0 . takeWhile (> 0) . iterate (`div` 2)
  where
    zero a x
      | x `mod` 2 == 0 = a + 1
      | otherwise = a

-- | Get the block number of the start of the era whose properties this object
-- describes that `current` belongs to.
birth :: (Integral a, Integral b) => Era -> a -> b
birth ImmortalEra _ = 0
birth (MortalEra period phase) current = fromIntegral $
    (max (fromIntegral current) phase - phase) `div` period * period + phase

-- | Get the block number of the first block at which the era has ended.
death :: (Integral a, Integral b, Bounded b) => Era -> a -> b
death ImmortalEra _                  = maxBound
death e@(MortalEra period _) current = fromIntegral $ birth e current + period
