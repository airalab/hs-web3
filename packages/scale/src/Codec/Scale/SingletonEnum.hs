-- |
-- Module      :  Codec.Scale.SingletonEnum
-- Copyright   :  Aleksandr Krupenkin 2016-2024
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  noportable
--
-- This type helps to encode/decode singleton Rust enums like:
-- `enum Enum { Data { some_data: u32 } }`
--

module Codec.Scale.SingletonEnum (SingletonEnum(..)) where

import           Data.Serialize.Get (getWord8)
import           Data.Serialize.Put (putWord8)

import           Codec.Scale.Class  (Decode (..), Encode (..))

-- | Haskell don't permit to make Rust-like enum type with only one element.
-- For this reason it is impossible to make generic parser for singleton enum type.
-- This type helps to parse Rust encoded singleton enums.
newtype SingletonEnum a = SingletonEnum { unSingletonEnum :: a }

instance Encode a => Encode (SingletonEnum a) where
    put (SingletonEnum x) = putWord8 0 >> put x

instance Decode a => Decode (SingletonEnum a) where
    get = getWord8 >> (SingletonEnum <$> get)
