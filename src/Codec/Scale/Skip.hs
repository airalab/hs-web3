-- |
-- Module      :  Codec.Scale.Skip
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  noportable
--
-- This type helps to skip fields in encoded data type.
--

module Codec.Scale.Skip (Skip(..)) where

import           Data.Default      (Default (..))

import           Codec.Scale.Class (Decode (..), Encode (..))

-- | This type hide filed from encoding context.
-- It's useful in cases when serialization impossible or not needed.
-- For decoding wrapped type should have 'Default' instance.
newtype Skip a = Skip { unSkip :: a }
  deriving (Eq, Ord, Show)

instance Encode (Skip a) where
    put _ = return ()

instance Default a => Decode (Skip a) where
    get = return def

instance Default a => Default (Skip a) where
    def = Skip def
