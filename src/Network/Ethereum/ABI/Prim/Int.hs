{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

-- |
-- Module      :  Network.Ethereum.Encoding.Prim.Int
-- Copyright   :  Alexander Krupenkin 2016-2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  noportable
--
-- Ethereum ABI int<N> and uint<N> types.
--

module Network.Ethereum.ABI.Prim.Int (
    UIntN
  , IntN
  , Word256
  , getWord256
  , putWord256
  ) where

import qualified Basement.Numerical.Number  as Basement
import           Basement.Types.Word256     (Word256 (..))
import qualified Basement.Types.Word256     as W
import           Data.Bits                  (Bits)
import           Data.Serialize             (Get, Putter, Serialize (..))
import           GHC.Generics               (Generic)
import           GHC.TypeLits

import           Network.Ethereum.ABI.Class (ABIGet (..), ABIPut (..),
                                             ABIType (..))

newtype UIntN (n :: Nat) = UIntN { unUIntN :: Word256 }
    deriving (Eq, Ord, Enum, Num, Real, Integral, Bits, Generic)

instance (KnownNat n, n <= 256) => Show (UIntN n) where
    show = show . unUIntN

instance (KnownNat n, n <= 256) => ABIType (UIntN n) where
    isDynamic _ = False

instance (KnownNat n, n <= 256) => ABIGet (UIntN n) where
    abiGet = UIntN <$> getWord256

instance (KnownNat n, n <= 256) => ABIPut (UIntN n) where
    abiPut = putWord256 . unUIntN

-- TODO: Signed data type
newtype IntN (n :: Nat) = IntN { unIntN :: Word256 }
    deriving (Eq, Ord, Enum, Num, Real, Integral, Bits, Generic)

instance (KnownNat n, n <= 256) => Show (IntN n) where
    show = show . toInteger

instance (KnownNat n, n <= 256) => ABIType (IntN n) where
    isDynamic _ = False

instance (KnownNat n, n <= 256) => ABIGet (IntN n) where
    abiGet = IntN <$> getWord256

instance (KnownNat n, n <= 256) => ABIPut (IntN n) where
    abiPut = putWord256 . unIntN

instance Serialize Word256 where
    put = putWord256
    get = getWord256

instance Real Word256 where
    toRational = toRational . toInteger

instance Integral Word256 where
    toInteger = Basement.toInteger
    quotRem a b = (W.quot a b, W.rem a b)

instance ABIType Integer where
    isDynamic _ = False

instance ABIPut Integer where
    abiPut = abiPut . toInt256

instance ABIGet Integer where
    abiGet = fmap (toInteger . fromInt256) abiGet

coerce :: (KnownNat a, KnownNat b, a <= b)
       => IntN a -> IntN b
coerce = undefined

toInt256 :: Integral a => a -> IntN 256
toInt256 x | x >= 0 = IntN (fromIntegral x)
           | otherwise = undefined

fromInt256 :: Integral a => IntN 256 -> a
fromInt256 = undefined

putWord256 :: Putter Word256
putWord256 (Word256 a3 a2 a1 a0) = put a3 >> put a2 >> put a1 >> put a0

getWord256 :: Get Word256
getWord256 = Word256 <$> get <*> get <*> get <*> get
