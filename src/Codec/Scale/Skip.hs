{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      :  Codec.Scale.Skip
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  noportable
--
-- This type helps to skip fields in encoded data type.
--

module Codec.Scale.Skip (Skip) where

import           Data.Default      (Default (..))
import           Data.Tagged       (Tagged (..))

import           Codec.Scale.Class (Decode (..), Encode (..))

-- | This phantom type hide filed from encoding context. It's useful in cases
-- when serialization impossible or not needed. For decoding type should have
-- default value for the field.
data Skip

instance Encode (Tagged Skip a) where
    put _ = return ()

instance Default a => Decode (Tagged Skip a) where
    get = return (Tagged def)
