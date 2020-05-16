{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Polkadot.Api.Babe
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Polkadot RPC methods with `babe` prefix.
--

module Network.Polkadot.Api.Babe where

import           Data.Aeson                 (Object)

import           Network.JsonRpc.TinyClient (JsonRpc (..))

-- | Returns data about which slots (primary or secondary) can be claimed
-- in the current epoch with the keys in the keystore.
epochAuthorship :: JsonRpc m => m Object
{-# INLINE epochAuthorship #-}
epochAuthorship = remote "babe_epochAuthorship"
