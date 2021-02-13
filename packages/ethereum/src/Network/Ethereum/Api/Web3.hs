{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Ethereum.Api.Web3
-- Copyright   :  Aleksandr Krupenkin 2016-2021
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unknown
--
-- Ethereum node JSON-RPC API methods with `web3_` prefix.
--

module Network.Ethereum.Api.Web3 where

import           Data.ByteArray.HexString   (HexString)
import           Data.Text                  (Text)
import           Network.JsonRpc.TinyClient (JsonRpc (..))

-- | Returns current node version string.
clientVersion :: JsonRpc m => m Text
{-# INLINE clientVersion #-}
clientVersion = remote "web3_clientVersion"

-- | Returns Keccak-256 (not the standardized SHA3-256) of the given data.
sha3 :: JsonRpc m => HexString -> m HexString
{-# INLINE sha3 #-}
sha3 = remote "web3_sha3"
