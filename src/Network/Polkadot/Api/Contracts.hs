{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Polkadot.Api.Contracts
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Polkadot RPC methods with `contracts` prefix.
--

module Network.Polkadot.Api.Contracts where

import           Data.Text                  (Text)

import           Data.ByteArray.HexString   (HexString)
import           Network.JsonRpc.TinyClient (JsonRpc (..))

-- | Executes a call to a contract.
call :: JsonRpc m
     => ContractCallRequest
     -- ^ Contract call
     -> Maybe HexString
     -- ^ Block hash or nothing for head
     -> ContractExecResult
{-# INLINE call #-}
call = remote "contracts_call"

-- | Returns the value under a specified storage key in a contract
getStorage :: JsonRpc m
           => Text
           -- ^ AccountId
           -> HexString
           -- ^ Storage key
           -> Maybe HexString
           -- ^ Block hash or nothing for head
           -> Maybe HexString
{-# INLINE getStorage #-}
getStorage = remote "contracts_getStorage"

-- | Returns the projected time a given contract will be able to sustain paying its rent.
rentProjection :: JsonRpc
               => Text
               -- ^ AccountId
               -> Maybe HexString
               -- ^ Block hash or nothing for head
               -> Maybe Int
{-# INLINE rentProjection #-}
rentProjection = remote "contracts_rentProjection"
