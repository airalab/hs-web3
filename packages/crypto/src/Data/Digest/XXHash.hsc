{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      :  Data.Digest.XXHash
-- Copyright   :  Aleksandr Krupenkin 2016-2020
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- xxHash C library bindings.
--
-- Variable bitLength implementation corresponds to polkadot-util:
-- https://github.com/polkadot-js/common/tree/master/packages/util-crypto/src/xxhash
--

module Data.Digest.XXHash (xxhash) where

import           Data.ByteString         (ByteString, useAsCStringLen)
import           Data.ByteString.Builder (toLazyByteString, word64LE)
import qualified Data.ByteString.Lazy    as LBS (ByteString)
import           Foreign
import           Foreign.C.String
import           Foreign.C.Types
import           System.IO.Unsafe        (unsafePerformIO)

#include <xxhash.h>

foreign import ccall unsafe "xxhash.h XXH64"
  c_XXH64 :: CString -> CSize -> CUInt -> IO Word64

xxhash_64 :: CUInt -> ByteString -> Word64
xxhash_64 seed = unsafePerformIO . flip useAsCStringLen
    (\(str, len) -> c_XXH64 str (fromIntegral len) seed)

-- | Create the xxhash64 and return the result with the specified 'bitLength'.
xxhash :: Integral bitLength
       => bitLength
       -- ^ Bit lenght of output, will be ceiling to 64 bit.
       -> ByteString
       -- ^ Input data.
       -> LBS.ByteString
       -- ^ Output hash.
xxhash bitLength input = toLazyByteString $ mconcat
    [ word64LE (xxhash_64 seed input) | seed <- [0 .. (iterations - 1)]]
  where
    iterations = ceiling (fromIntegral bitLength / 64)
