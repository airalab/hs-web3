{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Ethereum.Ens
-- Copyright   :  Alexander Krupenkin 2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- ENS offers a secure & decentralised way to address resources both on
-- and off the blockchain using simple, human-readable names.
--
-- This module provide basic ENS resolvers.
--

module Network.Ethereum.Ens where

import           Crypto.Hash                         (Digest, Keccak_256, hash)
import           Data.ByteArray                      (convert, zero)
import           Data.ByteArray.Sized                (unsafeFromByteArrayAccess)
import           Data.ByteString                     (ByteString)
import           Data.ByteString.Char8               (split)
import           Data.Default                        (def)
import           Data.Solidity.Prim                  (Address, BytesN)
import           Network.Ethereum.Api.Provider       (Web3)
import           Network.Ethereum.Api.Types          (Call (callTo),
                                                      DefaultBlock (Latest))
import qualified Network.Ethereum.Ens.PublicResolver as Resolver
import qualified Network.Ethereum.Ens.Registry       as Reg

-- | Namehash algorithm implementation: http://docs.ens.domains/en/latest/implementers.html#algorithm
namehash :: ByteString -> BytesN 32
namehash =
    unsafeFromByteArrayAccess . foldr algo (zero 32) . split '.'
  where
    algo a b = sha3 (b <> sha3 a)
    sha3 :: ByteString -> ByteString
    sha3 bs = convert (hash bs :: Digest Keccak_256)

-- | Get address of ENS domain
resolve :: ByteString -> Web3 Address
resolve name = do
    r <- Reg.resolver (def { callTo = Just ensRegistry }) Latest node
    Resolver.addr (def { callTo = Just r }) Latest node
  where
    node = namehash name
    ensRegistry = "0x314159265dD8dbb310642f98f50C066173C1259b"
