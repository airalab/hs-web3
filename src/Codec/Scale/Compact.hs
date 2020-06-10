-- |
-- Module      :  Codec.Scale.Compact
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  noportable
--
-- Efficient general integer codec.
--

module Codec.Scale.Compact (Compact(..)) where

import           Control.Monad      (replicateM)
import           Data.Bits          (shiftL, shiftR, (.&.), (.|.))
import           Data.List          (unfoldr)
import           Data.Serialize.Get (getWord16le, getWord32le, getWord8,
                                     lookAhead)
import           Data.Serialize.Put (putWord16le, putWord32le, putWord8)

import           Codec.Scale.Class  (Decode (..), Encode (..))

-- | A "compact" or general integer encoding is sufficient for encoding
-- large integers (up to 2**536) and is more efficient at encoding most
-- values than the fixed-width version.
newtype Compact a = Compact { unCompact :: a }
  deriving (Eq, Ord)

instance Show a => Show (Compact a) where
    show = ("Compact " ++) . show . unCompact

instance Integral a => Encode (Compact a) where
    put (Compact x)
      | n < 0 = error "negatives not supported by compact codec"
      | n < 64 = singleByteMode
      | n < 2^14 = twoByteMode
      | n < 2^30 = fourByteMode
      | n < 2^536 = bigIntegerMode
      | otherwise = error $ "unable to encode " ++ show n ++ " as compact"
      where
        n = toInteger x
        singleByteMode = putWord8 (fromIntegral x `shiftL` 2)
        twoByteMode = putWord16le (fromIntegral x `shiftL` 2 .|. 1)
        fourByteMode = putWord32le (fromIntegral x `shiftL` 2 .|. 2)
        bigIntegerMode = do
            let step 0 = Nothing
                step i = Just (fromIntegral i, i `shiftR` 8)
                unroll = unfoldr step n
            putWord8 (fromIntegral (length unroll) `shiftL` 2 .|. 3)
            mapM_ putWord8 unroll

instance Integral a => Decode (Compact a) where
    get = do
        mode <- lookAhead ((3 .&.) <$> getWord8)
        Compact <$> case mode of
          0 -> fromIntegral <$> singleByteMode
          1 -> fromIntegral <$> twoByteMode
          2 -> fromIntegral <$> fourByteMode
          3 -> bigIntegerMode
          _ -> fail "unexpected prefix decoding compact number"
      where
        singleByteMode = flip shiftR 2 <$> getWord8
        twoByteMode = flip shiftR 2 <$> getWord16le
        fourByteMode = flip shiftR 2 <$> getWord32le
        bigIntegerMode = do
            let unstep b a = a `shiftL` 8 .|. fromIntegral b
                roll = fromInteger . foldr unstep 0
            len <- flip shiftR 2 <$> getWord8
            roll <$> replicateM (fromIntegral len) getWord8
