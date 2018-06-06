{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Ethereum.Web3.Web3
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unknown
--
-- Ethereum node JSON-RPC API methods with `web3_` prefix.
--

module Network.Ethereum.Web3.Web3 where

import           Data.Text                       (Text)
import           Network.Ethereum.ABI.Prim.Bytes (Bytes)
import           Network.Ethereum.Web3.Provider  (Web3)
import           Network.Ethereum.Web3.Types     (Hash)
import           Network.JsonRpc.TinyClient      (remote)

-- | Returns current node version string.
clientVersion :: Web3 Text
{-# INLINE clientVersion #-}
clientVersion = remote "web3_clientVersion"

-- | Returns Keccak-256 (not the standardized SHA3-256) of the given data.
sha3 :: Bytes -> Web3 Hash
{-# INLINE sha3 #-}
sha3 = remote "web3_sha3"
