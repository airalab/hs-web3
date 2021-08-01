{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      :  Network.Polkadot.Extrinsic.Era
-- Copyright   :  Aleksandr Krupenkin 2016-2021
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
--
--

module Network.Polkadot.Extrinsic.Era
    ( Era(..)
    , mkMortal
    ) where

import           Codec.Scale.Class (Decode (..), Encode (..))
import           Codec.Scale.Core  ()
import           Data.Bits         (shiftL, shiftR)
import           Data.Word         (Word16, Word8, byteSwap16)

-- | The era for an extrinsic, indicating either a mortal or immortal extrinsic.
data Era
  = ImmortalEra
  -- ^ The ImmortalEra for an extrinsic.
  | MortalEra
  -- ^ The MortalEra for an extrinsic, indicating period and phase.
    { mortalEraPeriod :: Int
    , mortalEraPhase  :: Int
    }
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
             in mkMortal (first' + second' `shiftL` 8)

instance Encode Era where
    put ImmortalEra = put (0 :: Word8)
    put MortalEra{..} = put encoded
      where
        encoded :: Word16
        encoded = min 15 (max 1 (getTrailingZeros period - 1)) + ((phase `div` quantizeFactor) `shiftL` 4)
        quantizeFactor = max (period `shiftR` 12) 1
        period = fromIntegral mortalEraPeriod
        phase = fromIntegral mortalEraPhase

mkMortal :: Word16 -> Era
mkMortal raw = MortalEra period phase
  where
    encoded = byteSwap16 raw
    era = fromIntegral encoded
    period = 2 `shiftL` (era `rem` 16)
    quantizeFactor = max (period `shiftR` 12) 1
    phase = (era `shiftR` 4) * quantizeFactor

getTrailingZeros :: Integral a => a -> a
getTrailingZeros = foldl zero 0 . takeWhile (> 0) . iterate (`div` 2)
  where
    zero a x
      | x `mod` 2 == 0 = a + 1
      | otherwise = a
