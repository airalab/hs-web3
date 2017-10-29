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

import Network.Ethereum.Web3.Provider
import Network.Ethereum.Web3.JsonRpc
import Network.Ethereum.Web3.Types
import Data.Text (Text)

-- | Returns current node version string.
clientVersion :: Provider a => Web3 a Text
{-# INLINE web3_clientVersion #-}
clientVersion = remote "web3_clientVersion"

-- | Returns Keccak-256 (not the standardized SHA3-256) of the given data.
sha3 :: Provider a => Text -> Web3 a Text
{-# INLINE web3_sha3 #-}
sha3 = remote "web3_sha3"
