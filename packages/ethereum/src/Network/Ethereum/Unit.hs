{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Network.Ethereum.Unit
-- Copyright   :  Aleksandr Krupenkin 2016-2021
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- Ethereum has a metric system of denominations used as units of ether.
-- Each denomination has its own unique name (some bear the family name
-- of seminal figures playing a role in evolution of computer science
-- and cryptoeconomics). The smallest denomination aka base unit of ether
-- is called 'Wei'. Below is a list of the named denominations and their
-- value in 'Wei'. Following a common (although somewhat ambiguous) pattern,
-- ether also designates a unit (of 1e18 or one quintillion 'Wei') of the
-- currency. Note that the currency is not called Ethereum as many mistakenly
-- think, nor is Ethereum a unit.
--
-- In Haskell the Ethereum unit system presented as set of types: 'Wei',
-- 'Szabo', 'Finney', etc. They are members of 'Unit' typeclass. Also available
-- standart 'Show', 'Read', 'Num' operations over Ethereum units.
--
-- @
-- > let x = 1.2 :: Ether
-- > toWei x
-- 1200000000000000000
--
-- > let y = x + 2
-- > y
-- 3.20 ether
--
-- > let z = 15 :: Szabo
-- > y + z
--
-- <interactive>:6:5: error:
--    • Couldn't match type ‘Network.Ethereum.Unit.U4’
--                    with ‘Network.Ethereum.Unit.U6’
--      Expected type: Ether
--      Actual type: Szabo
-- @
--

module Network.Ethereum.Unit
    (
    -- * The @Unit@ type class
      Unit(..)

    -- * Ethereum value metrics
    , Wei
    , Babbage
    , Lovelace
    , Shannon
    , Szabo
    , Finney
    , Ether
    , KEther
    ) where

import           Data.Proxy                      (Proxy (..))
import           Data.Text.Lazy                  (Text, unpack)
import           GHC.Generics                    (Generic)
import           GHC.Read
import           Text.ParserCombinators.ReadPrec
import           Text.Printf
import qualified Text.Read.Lex                   as L

-- | Ethereum value unit
class (Read a, Show a, UnitSpec a, Fractional a) => Unit a where
    -- | Make a value from integer wei
    fromWei :: Integral b => b -> a

    -- | Convert a value to integer wei
    toWei :: Integral b => a -> b

-- | Unit specification
class UnitSpec a where
    divider :: RealFrac b => proxy a -> b
    name    :: proxy a -> Text

-- | Value abstraction
newtype Value a = MkValue { unValue :: Integer }
  deriving (Eq, Ord, Generic)

mkValue :: forall a b . (UnitSpec a, RealFrac b) => b -> Value a
mkValue = MkValue . round . (* divider (Proxy :: Proxy a))

instance UnitSpec a => Unit (Value a) where
    fromWei = MkValue . toInteger
    toWei   = fromInteger . unValue

instance UnitSpec a => UnitSpec (Value a) where
    divider = const $ divider (Proxy :: Proxy a)
    name    = const $ name (Proxy :: Proxy a)

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
    show val = printf "%F %s" (x / d :: Double) (name val)
      where
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

-- | Babbage unit type
type Babbage = Value U1

instance UnitSpec U1 where
    divider = const 1e3
    name    = const "babbage"

-- | Lovelace unit type
type Lovelace = Value U2

instance UnitSpec U2 where
    divider = const 1e6
    name    = const "lovelace"

-- | Shannon unit type
type Shannon = Value U3

instance UnitSpec U3 where
    divider = const 1e9
    name    = const "shannon"

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
