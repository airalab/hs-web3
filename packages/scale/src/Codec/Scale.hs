{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      :  Codec.Scale.Class
-- Copyright   :  Aleksandr Krupenkin 2016-2024
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- The SCALE (Simple Concatenated Aggregate Little-Endian) Codec is
-- a lightweight, efficient, binary serialization and deserialization codec.
--
-- It is designed for high-performance, copy-free encoding and decoding of data in
-- resource-constrained execution contexts, like the Substrate runtime. It is not
-- self-describing in any way and assumes the decoding context has all type
-- knowledge about the encoded data.
--

module Codec.Scale
    ( encode
    , decode
    , encode'
    , decode'
    , Encode
    , Decode
    , Generic
    , module Core
    ) where

import           Data.ByteArray    (ByteArray, ByteArrayAccess, convert)
import           Data.Serialize    (runGet, runPut)
import           Generics.SOP      (Generic, Rep, from, to)

import           Codec.Scale.Class (Decode (..), Encode (..), GDecode (..),
                                    GEncode (..))
import           Codec.Scale.Core  as Core

-- | Encode datatype to SCALE format.
encode :: (Encode a, ByteArray ba)
       => a
       -> ba
{-# INLINE encode #-}
encode = convert . runPut . put

-- | Generic driven version of 'encode'
encode' :: (Generic a,
            Rep a ~ rep,
            GEncode rep,
            ByteArray ba)
        => a
        -> ba
{-# INLINE encode' #-}
encode' = convert . runPut . gPut . from

-- | Decode datatype from SCALE format.
decode :: (ByteArrayAccess ba, Decode a)
       => ba
       -> Either String a
{-# INLINE decode #-}
decode = runGet get . convert

-- | Generic driven version of 'decode'
decode' :: (Generic a,
            Rep a ~ rep,
            GDecode rep,
            ByteArrayAccess ba)
        => ba
        -> Either String a
{-# INLINE decode' #-}
decode' = runGet (to <$> gGet) . convert
