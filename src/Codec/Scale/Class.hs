{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}

-- |
-- Module      :  Codec.Scale.Class
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
--
--

module Codec.Scale.Class where

import           Data.Serialize (Get, Putter)
import           Generics.SOP   (Generic, Rep, from, to)

-- | A class for encoding datatypes to SCALE format.
--
-- If your compiler has support for the @DeriveGeneric@ and
-- @DefaultSignatures@ language extensions (@ghc >= 7.2.1@),
-- the 'put' method will have default generic implementations.
--
-- To use this option, simply add a @deriving 'Generic'@ clause
-- to your datatype and declare a 'Encode' instance for it without
-- giving a definition for 'put'.
--
class Encode a where
    put :: Putter a

    default put :: (Generic a, Rep a ~ rep, GEncode rep) => Putter a
    put = gPut . from

-- | A class for encoding generically composed datatypes to SCALE format.
class GEncode a where
    gPut :: Putter a

-- | A class for decoding datatypes from SCALE format.
--
-- If your compiler has support for the @DeriveGeneric@ and
-- @DefaultSignatures@ language extensions (@ghc >= 7.2.1@),
-- the 'get' method will have default generic implementations.
--
-- To use this option, simply add a @deriving 'Generic'@ clause
-- to your datatype and declare a 'Decode' instance for it without
-- giving a definition for 'get'.
--
class Decode a where
    get :: Get a

    default get :: (Generic a, Rep a ~ rep, GDecode rep) => Get a
    get = to <$> gGet

-- | A class for decoding generically composed datatypes from SCALE format.
class GDecode a where
    gGet :: Get a
