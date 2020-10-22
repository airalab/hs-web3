{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Polkadot.Rpc.Babe
-- Copyright   :  Aleksandr Krupenkin 2016-2020
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Polkadot RPC methods with `babe` prefix.
--

module Network.Polkadot.Rpc.Babe where

import           Data.Aeson                 (Object)
import           Network.JsonRpc.TinyClient (JsonRpc (..))

-- | Returns data about which slots (primary or secondary) can be claimed
-- in the current epoch with the keys in the keystore.
epochAuthorship :: JsonRpc m => m Object
{-# INLINE epochAuthorship #-}
epochAuthorship = remote "babe_epochAuthorship"
