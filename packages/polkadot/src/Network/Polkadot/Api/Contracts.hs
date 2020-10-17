{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Polkadot.Api.Contracts
-- Copyright   :  Aleksandr Krupenkin 2016-2020
-- License     :  Apache-2.0
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
import           Network.Polkadot.Api.Types (ContractCall, ContractExecResult)

-- | Executes a call to a contract.
call :: JsonRpc m
     => ContractCall
     -- ^ Contract call
     -> Maybe HexString
     -- ^ Block hash or nothing for head
     -> m ContractExecResult
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
           -> m (Maybe HexString)
{-# INLINE getStorage #-}
getStorage = remote "contracts_getStorage"

-- | Returns the projected time a given contract will be able to sustain paying its rent.
rentProjection :: JsonRpc m
               => Text
               -- ^ AccountId
               -> Maybe HexString
               -- ^ Block hash or nothing for head
               -> m (Maybe Int)
{-# INLINE rentProjection #-}
rentProjection = remote "contracts_rentProjection"
