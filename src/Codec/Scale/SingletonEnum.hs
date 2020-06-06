{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      :  Codec.Scale.SingletonEnum
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  noportable
--
-- This type helps to encode/decode singleton Rust enums like:
-- `enum Enum { Data { some_data: u32 } }`
--

module Codec.Scale.SingletonEnum (SingletonEnum) where

import           Data.Serialize.Get (getWord8)
import           Data.Serialize.Put (putWord8)
import           Data.Tagged        (Tagged (..))

import           Codec.Scale.Class  (Decode (..), Encode (..))

-- | Haskell don't permit to make Rust-like enum type with only one element.
-- For this reason it is impossible to make generic parser for singleton enum type.
-- This phantom type helps to parse Rust encoded singleton enums.
data SingletonEnum

instance Encode a => Encode (Tagged SingletonEnum a) where
    put x = putWord8 0 >> put (unTagged x)

instance Decode a => Decode (Tagged SingletonEnum a) where
    get = getWord8 >> (Tagged <$> get)
