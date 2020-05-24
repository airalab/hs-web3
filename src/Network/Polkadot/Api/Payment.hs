{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Polkadot.Api.Payment
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Polkadot RPC methods with `payment` prefix.
--

module Network.Polkadot.Api.Payment where

import           Data.ByteArray.HexString   (HexString)
import           Network.JsonRpc.TinyClient (JsonRpc (..))
import           Network.Polkadot.Api.Types (RuntimeDispatchInfo)

-- | Retrieves the fee information for an encoded extrinsic.
queryInfo :: JsonRpc m
          => HexString
          -- ^ Extrinsic
          -> Maybe HexString
          -- ^ Block hash or nothing for head block
          -> m RuntimeDispatchInfo
{-# INLINE queryInfo #-}
queryInfo = remote "payment_queryInfo"
