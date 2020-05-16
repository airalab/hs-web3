-- |
-- Module      :  Crypto.Ethereum.Utils
-- Copyright   :  Alexander Krupenkin 2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Ethereum crypto module helper functions.
--

module Crypto.Ethereum.Utils where

import           Crypto.Hash    (Keccak_256 (..), hashWith)
import           Data.ByteArray (ByteArray, ByteArrayAccess, convert)

-- | Keccak 256 hash function.
keccak256 :: (ByteArrayAccess bin, ByteArray bout) => bin -> bout
{-# INLINE keccak256 #-}
keccak256 = convert . hashWith Keccak_256
