-- |
-- Module      :  Codec.Scale.Compact
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
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
    show = ("Compact " ++) . show

instance Integral a => Encode (Compact a) where
    put (Compact x)
      | x >= 0 && x < 64 = singleByteMode x
      | x >= 64 && x < (2^14-1) = twoByteMode x
      | x >= (2^14-1) && x < (2^30-1) = fourByteMode x
      | x >= (2^30-1) && x < (2^536-1) = bigIntegerMode x
      | otherwise = error $ show (toInteger x) ++ " could not be encoded as compact number"
      where
        singleByteMode a = putWord8 (fromIntegral a `shiftL` 2)
        twoByteMode a = putWord16le (fromIntegral a `shiftL` 2 .|. 1)
        fourByteMode a = putWord32le (fromIntegral a `shiftL` 2 .|. 2)
        bigIntegerMode a = do
            let step 0 = Nothing
                step i = Just (fromIntegral i, i `shiftR` 8)
                unroll = unfoldr step (toInteger a)
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
