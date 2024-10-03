{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Polkadot.Rpc.Payment
-- Copyright   :  Aleksandr Krupenkin 2016-2024
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Polkadot RPC methods with `payment` prefix.
--

module Network.Polkadot.Rpc.Payment where

import           Data.ByteArray.HexString   (HexString)
import           Network.JsonRpc.TinyClient (JsonRpc (..))

import           Network.Polkadot.Rpc.Types (RuntimeDispatchInfo)

-- | Retrieves the fee information for an encoded extrinsic.
queryInfo :: JsonRpc m
          => HexString
          -- ^ Extrinsic
          -> Maybe HexString
          -- ^ Block hash or nothing for head block
          -> m RuntimeDispatchInfo
{-# INLINE queryInfo #-}
queryInfo = remote "payment_queryInfo"
