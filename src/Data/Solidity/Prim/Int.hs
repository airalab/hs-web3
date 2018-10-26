{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

-- |
-- Module      :  Data.Solidity.Prim.Int
-- Copyright   :  Alexander Krupenkin 2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  noportable
--
-- Ethereum Abi intN and uintN types.
--

module Data.Solidity.Prim.Int
    (
    -- * The @IntN@ type
      IntN

    -- * The @UIntN@ type
    , UIntN

    -- * @Word256@ serializers
    , getWord256
    , putWord256
    ) where

import qualified Basement.Numerical.Number as Basement (toInteger)
import           Basement.Types.Word256    (Word256 (Word256))
import qualified Basement.Types.Word256    as Basement (quot, rem)
import           Data.Bits                 (Bits (testBit), (.&.))
import           Data.Proxy                (Proxy (..))
import           Data.Serialize            (Get, Putter, Serialize (get, put))
import           GHC.Generics              (Generic)
import           GHC.TypeLits

import           Data.Solidity.Abi         (AbiGet (..), AbiPut (..),
                                            AbiType (..))

instance Real Word256 where
    toRational = toRational . toInteger

instance Integral Word256 where
    toInteger = Basement.toInteger
    quotRem a b = (Basement.quot a b, Basement.rem a b)

-- | Unsigned integer with fixed length in bits
newtype UIntN (n :: Nat) = UIntN { unUIntN :: Word256 }
    deriving (Eq, Ord, Enum, Bits, Generic)

instance (KnownNat n, n <= 256) => Num (UIntN n) where
    a + b  = fromInteger (toInteger a + toInteger b)
    a - b  = fromInteger (toInteger a - toInteger b)
    a * b  = fromInteger (toInteger a * toInteger b)
    abs    = fromInteger . abs . toInteger
    negate = fromInteger . negate . toInteger
    signum = fromInteger . signum . toInteger
    fromInteger x
      | x >= 0 = mask $ UIntN (fromInteger x)
      | otherwise = mask $ UIntN (fromInteger $ 2 ^ 256 + x)
      where
        mask = (maxBound .&.) :: UIntN n -> UIntN n

instance (KnownNat n, n <= 256) => Show (UIntN n) where
    show = show . unUIntN

instance (KnownNat n, n <= 256) => Bounded (UIntN n) where
    minBound = UIntN 0
    maxBound = UIntN $ 2 ^ natVal (Proxy :: Proxy n) - 1

instance (KnownNat n, n <= 256) => Real (UIntN n) where
    toRational = toRational . toInteger

instance (KnownNat n, n <= 256) => Integral (UIntN n) where
    toInteger = toInteger . unUIntN
    quotRem (UIntN a) (UIntN b) = (UIntN $ quot a b, UIntN $ rem a b)

instance (n <= 256) => AbiType (UIntN n) where
    isDynamic _ = False

instance (n <= 256) => AbiGet (UIntN n) where
    abiGet = UIntN <$> getWord256

instance (n <= 256) => AbiPut (UIntN n) where
    abiPut = putWord256 . unUIntN

-- Signed integer with fixed length in bits
newtype IntN (n :: Nat) = IntN { unIntN :: Word256 }
    deriving (Eq, Ord, Enum, Bits, Generic)

instance (KnownNat n, n <= 256) => Show (IntN n) where
    show = show . toInteger

instance (KnownNat n, n <= 256) => Bounded (IntN n) where
    minBound = IntN $ negate $ 2 ^ (natVal (Proxy :: Proxy (n :: Nat)) - 1)
    maxBound = IntN $ 2 ^ (natVal (Proxy :: Proxy (n :: Nat)) - 1) - 1

instance (KnownNat n, n <= 256) => Num (IntN n) where
    a + b  = fromInteger (toInteger a + toInteger b)
    a - b  = fromInteger (toInteger a - toInteger b)
    a * b  = fromInteger (toInteger a * toInteger b)
    abs    = fromInteger . abs . toInteger
    negate = fromInteger . negate . toInteger
    signum = fromInteger . signum . toInteger
    fromInteger x
      | x >= 0 = IntN (fromInteger x)
      | otherwise = IntN (fromInteger $ 2 ^ 256 + x)

instance (KnownNat n, n <= 256) => Real (IntN n) where
    toRational = toRational . toInteger

instance (KnownNat n, n <= 256) => Integral (IntN n) where
    quotRem (IntN a) (IntN b) = (IntN $ quot a b, IntN $ rem a b)
    toInteger x
      | testBit x 255 = toInteger (unIntN x) - 2 ^ 256
      | otherwise = toInteger $ unIntN x

instance (n <= 256) => AbiType (IntN n) where
    isDynamic _ = False

instance (n <= 256) => AbiGet (IntN n) where
    abiGet = IntN <$> getWord256

instance (n <= 256) => AbiPut (IntN n) where
    abiPut = putWord256 . unIntN

putWord256 :: Putter Word256
putWord256 (Word256 a3 a2 a1 a0) =
    put a3 >> put a2 >> put a1 >> put a0

getWord256 :: Get Word256
getWord256 = Word256 <$> get <*> get <*> get <*> get
