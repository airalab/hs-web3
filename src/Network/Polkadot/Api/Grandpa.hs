{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Polkadot.Api.Grandpa
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Polkadot RPC methods with `grandpa` prefix.
--

module Network.Polkadot.Api.Grandpa where

import           Data.Aeson                 (Object)

import           Network.JsonRpc.TinyClient (JsonRpc (..))

-- | Returns the state of the current best round state as well as the ongoing background rounds.
roundState :: JsonRpc m => m Object
{-# INLINE roundState #-}
roundState = remote "grandpa_roundState"
