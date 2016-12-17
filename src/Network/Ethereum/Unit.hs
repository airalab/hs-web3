{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
-- |
-- Module      :  Network.Ethereum.Unit
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- Ethereum unit conversion utils.
--
module Network.Ethereum.Unit (
    Unit(..)
  , Wei
  , KWei
  , MWei
  , GWei
  , Szabo
  , Finney
  , Ether
  , KEther
  ) where

import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder.RealFloat
import Text.ParserCombinators.ReadPrec
import Data.Text.Lazy (Text, unpack)
import qualified Text.Read.Lex as L
import Data.Monoid ((<>))
import GHC.Read

-- | Ethereum value unit
class Unit a where
    -- | Make a value from integer wei
    fromWei :: Integer -> a
    -- | Convert a value to integer wei
    toWei :: a -> Integer
    -- | Conversion beween two values
    convert :: Unit b => a -> b
    {-# INLINE convert #-}
    convert = fromWei . toWei

-- | Unit specification
class UnitSpec a where
    divider :: RealFrac b => Value a -> b
    name    :: Value a -> Text

-- | Value abstraction
data Value a = MkValue { unValue :: Integer }
  deriving (Eq, Ord)

mkValue :: (UnitSpec a, RealFrac b) => b -> Value a
mkValue = modify res . round . (divider res *)
  where res = undefined :: UnitSpec a => Value a
        modify :: Value a -> Integer -> Value a
        modify _ = MkValue

instance Unit (Value a) where
    fromWei = MkValue
    toWei   = unValue

instance UnitSpec a => Num (Value a) where
   a + b = MkValue (unValue a + unValue b)
   a - b = MkValue (unValue a - unValue b)
   a * b = MkValue (unValue a * unValue b)

   signum (MkValue a) = MkValue (abs a)
   abs (MkValue a)    = MkValue (abs a)
   fromInteger        = mkValue . fromIntegral

instance UnitSpec a => Fractional (Value a) where
    a / b = MkValue (unValue a `div` unValue b)
    fromRational = mkValue

instance UnitSpec a => Show (Value a) where
    show val = unpack (toLazyText floatValue <> " " <> name val)
      where
        floatValue = formatRealFloat Fixed (Just 2) (x / d)
        x = fromIntegral (unValue val)
        d = divider val

instance UnitSpec a => Read (Value a) where
    readPrec = parens $ do
        x <- readPrec
        let res = mkValue x
            resName = unpack (name res)
        step $ expectP (L.Ident resName)
        return res

data U0
data U1
data U2
data U3
data U4
data U5
data U6
data U7

-- | Wei unit type
type Wei = Value U0

instance UnitSpec U0 where
    divider = const 1
    name    = const "wei"

-- | KWei unit type
type KWei = Value U1

instance UnitSpec U1 where
    divider = const 1e3
    name    = const "kwei"

-- | MWei unit type
type MWei = Value U2

instance UnitSpec U2 where
    divider = const 1e6
    name    = const "mwei"

-- | GWei unit type
type GWei = Value U3

instance UnitSpec U3 where
    divider = const 1e9
    name    = const "gwei"

-- | Szabo unit type
type Szabo = Value U4

instance UnitSpec U4 where
    divider = const 1e12
    name    = const "szabo"

-- | Finney unit type
type Finney = Value U5

instance UnitSpec U5 where
    divider = const 1e15
    name    = const "finney"

-- | Ether unit type
type Ether  = Value U6

instance UnitSpec U6 where
    divider = const 1e18
    name    = const "ether"

-- | KEther unit type
type KEther = Value U7

instance UnitSpec U7 where
    divider = const 1e21
    name    = const "kether"
