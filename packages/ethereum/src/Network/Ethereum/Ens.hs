{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Ethereum.Ens
-- Copyright   :  Aleksandr Krupenkin 2016-2021
-- License     :  Apache-2.0
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

import           Crypto.Ethereum                     (keccak256)
import           Data.ByteArray                      (zero)
import           Data.ByteArray.Sized                (unsafeFromByteArrayAccess)
import           Data.ByteString                     (ByteString)
import           Data.ByteString.Char8               (split)
import           Lens.Micro                          ((.~))

import           Data.Solidity.Prim                  (Address, BytesN)
import           Network.Ethereum.Account.Class      (Account)
import           Network.Ethereum.Account.Internal   (AccountT, to, withParam)
import qualified Network.Ethereum.Ens.PublicResolver as Resolver
import qualified Network.Ethereum.Ens.Registry       as Reg
import           Network.JsonRpc.TinyClient          (JsonRpc)

-- | Namehash algorithm
-- http://docs.ens.domains/en/latest/implementers.html#algorithm
namehash :: ByteString
         -- ^ Domain name
         -> BytesN 32
         -- ^ Associated ENS node
namehash =
    unsafeFromByteArrayAccess . foldr algo (zero 32) . split '.'
  where
    algo :: ByteString -> ByteString -> ByteString
    algo a b = keccak256 (b <> keccak256 a)

-- | Get address of ENS domain
resolve :: (JsonRpc m, Account p (AccountT p))
        => ByteString
        -- ^ Domain name
        -> AccountT p m Address
        -- ^ Associated address
resolve name = do
    r <- ensRegistry $ Reg.resolver node
    withParam (to .~ r) $ Resolver.addr node
  where
    node = namehash name
    ensRegistry = withParam $ to .~ "0x314159265dD8dbb310642f98f50C066173C1259b"
