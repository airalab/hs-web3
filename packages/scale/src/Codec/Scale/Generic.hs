{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeInType        #-}
{-# LANGUAGE TypeOperators     #-}

-- |
-- Module      :  Codec.Scale.Generic
-- Copyright   :  Aleksandr Krupenkin 2016-2024
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- This module defines generic codec instances for data structures (including tuples)
-- and enums (tagged-unions in Rust).
--

module Codec.Scale.Generic () where

import           Data.Serialize.Get (Get, getWord8)
import           Data.Serialize.Put (PutM, putWord8)
import           Data.Word          (Word8)
import           Generics.SOP       (All, Compose, I (..), NP (..), NS (..),
                                     SOP (..), unSOP, unZ)

import           Codec.Scale.Class  (Decode (..), Encode (..), GDecode (..),
                                     GEncode (..))

-- Enum has multiple sum types.
instance ( GEncode (NP f xs)
         , GEncode (NP f ys)
         , All (GEncode `Compose` NP f) xss
         ) => GEncode (SOP f (xs ': ys ': xss)) where
    gPut = go 0 . unSOP
      where
        go :: forall f as . All (GEncode `Compose` f) as => Word8 -> NS f as -> PutM ()
        go !acc (Z x) = putWord8 acc >> gPut x
        go !acc (S x) = go (acc + 1) x

-- Structures has only one sum type.
instance GEncode (NP f xs) => GEncode (SOP f '[xs]) where
    gPut = gPut . unZ . unSOP

-- Product serialization is just encode each field step by step.
instance (Encode a, GEncode (NP I as)) => GEncode (NP I (a ': as)) where
    gPut (I a :* as) = put a >> gPut as

-- Finish when all fields handled.
instance GEncode (NP I '[]) where
    gPut _ = mempty

-- | Enum parser definition.
--
-- The index of sum type to parse given as an argument.
class EnumParser xs where
    enumParser :: All (GDecode `Compose` NP f) xs => Word8 -> Get (NS (NP f) xs)

-- Enumerate enum index, zero means that we reach the goal.
instance EnumParser as => EnumParser (a ': as) where
    enumParser !i | i > 0     = S <$> enumParser (i - 1)
                  | otherwise = Z <$> gGet

-- When index out of type scope raise the error.
instance EnumParser '[] where
    enumParser i = fail ("index out of enum constructors count: " ++ show i)

-- Decode enum when multiple sum types.
instance ( GDecode (NP f xs)
         , GDecode (NP f ys)
         , All (GDecode `Compose` NP f) xss
         , EnumParser xss
         ) => GDecode (SOP f (xs ': ys ': xss)) where
    gGet = SOP <$> (enumParser =<< getWord8)

-- Decode plain structure when only one sum type.
instance GDecode (NP f as) => GDecode (SOP f '[as]) where
    gGet = SOP . Z <$> gGet

-- Decode each field in sequence.
instance (Decode a, GDecode (NP I as)) => GDecode (NP I (a ': as)) where
    gGet = (:*) <$> (I <$> get) <*> gGet

-- Finish decoding when empty.
instance GDecode (NP I '[]) where
    gGet = return Nil
