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
    IntN
  , UIntN
  , getWord256
  , putWord256
  ) where

import qualified Basement.Numerical.Number  as Basement (toInteger)
import           Basement.Types.Word256     (Word256 (Word256))
import qualified Basement.Types.Word256     as Basement (quot, rem)
import           Data.Bits                  (Bits)
import           Data.Serialize             (Get, Putter, Serialize (get, put))
import           GHC.Generics               (Generic)
import           GHC.TypeLits

import           Network.Ethereum.ABI.Class (ABIGet (..), ABIPut (..),
                                             ABIType (..))

instance Real Word256 where
    toRational = toRational . toInteger

instance Integral Word256 where
    toInteger = Basement.toInteger
    quotRem a b = (Basement.quot a b, Basement.rem a b)

newtype UIntN (n :: Nat) = UIntN { unUIntN :: Word256 }
    deriving (Eq, Ord, Enum, Num, Bits, Generic)

instance (KnownNat n, n <= 256) => Show (UIntN n) where
    show = show . unUIntN

instance (KnownNat n, n <= 256) => Real (UIntN n) where
    toRational = toRational . toInteger

instance (KnownNat n, n <= 256) => Integral (UIntN n) where
    toInteger = toInteger . unUIntN
    quotRem (UIntN a) (UIntN b) = (UIntN $ quot a b, UIntN $ rem a b)

instance (n <= 256) => ABIType (UIntN n) where
    isDynamic _ = False

instance (n <= 256) => ABIGet (UIntN n) where
    abiGet = UIntN <$> getWord256

instance (n <= 256) => ABIPut (UIntN n) where
    abiPut = putWord256 . unUIntN

-- TODO: Signed data type
newtype IntN (n :: Nat) = IntN { unIntN :: Word256 }
    deriving (Eq, Ord, Enum, Num, Bits, Generic)

instance (n <= 256) => Show (IntN n) where
    show = show . toInteger

instance (n <= 256) => Real (IntN n) where
    toRational = toRational . toInteger

instance (n <= 256) => Integral (IntN n) where
    toInteger = toInteger . unIntN
    quotRem (IntN a) (IntN b) = (IntN $ quot a b, IntN $ rem a b)

instance (n <= 256) => ABIType (IntN n) where
    isDynamic _ = False

instance (n <= 256) => ABIGet (IntN n) where
    abiGet = IntN <$> getWord256

instance (n <= 256) => ABIPut (IntN n) where
    abiPut = putWord256 . unIntN

{-
toInt256 :: Integral a => a -> IntN 256
toInt256 x | x >= 0 = IntN (fromIntegral x)
           | otherwise = undefined

fromInt256 :: Integral a => IntN 256 -> a
fromInt256 = undefined
-}

putWord256 :: Putter Word256
putWord256 (Word256 a3 a2 a1 a0) =
    put a3 >> put a2 >> put a1 >> put a0

getWord256 :: Get Word256
getWord256 = Word256 <$> get <*> get <*> get <*> get
