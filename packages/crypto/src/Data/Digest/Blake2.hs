{-# LANGUAGE DataKinds #-}

-- |
-- Module      :  Data.Digest.Blake2
-- Copyright   :  Aleksandr Krupenkin 2016-2020
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- Cryptonite Blake2b wrappers.
--

module Data.Digest.Blake2 where

import           Crypto.Hash          (Blake2b, Digest, hash)
import           Data.ByteArray       (convert)
import           Data.ByteString      (ByteString)
import           Data.ByteString.Lazy (fromStrict)
import qualified Data.ByteString.Lazy as LBS (ByteString)

-- | Blake2b with 64 bit output.
blake2_64 :: ByteString -> LBS.ByteString
{-# INLINE blake2_64 #-}
blake2_64 = fromStrict . convert . (hash :: ByteString -> Digest (Blake2b 64))

-- | Blake2b with 128 bit output.
blake2_128 :: ByteString -> LBS.ByteString
{-# INLINE blake2_128 #-}
blake2_128 = fromStrict . convert . (hash :: ByteString -> Digest (Blake2b 128))

-- | Blake2b with 256 bit output.
blake2_256 :: ByteString -> LBS.ByteString
{-# INLINE blake2_256 #-}
blake2_256 = fromStrict . convert . (hash :: ByteString -> Digest (Blake2b 256))

-- | Blake2b with 512 bit output.
blake2_512 :: ByteString -> LBS.ByteString
{-# INLINE blake2_512 #-}
blake2_512 = fromStrict . convert . (hash :: ByteString -> Digest (Blake2b 512))
