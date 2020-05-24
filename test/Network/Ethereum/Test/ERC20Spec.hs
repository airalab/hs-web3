{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}

-- |
-- Module      :  Network.Ethereum.Test.ERC20Spec
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
--

module Network.Ethereum.Test.ERC20Spec where

import           Test.Hspec

import           Network.Ethereum             (Account, UIntN)
import           Network.Ethereum.Contract.TH (abiFrom)
import           Network.JsonRpc.TinyClient   (JsonRpc)

[abiFrom|examples/token/ERC20.json|]

-- this spec is just to test compilation
spec :: Spec
spec = return ()

getBalance :: (JsonRpc m, Account p t, Functor (t m))
           => t m (UIntN 256)
getBalance = balanceOf "0x1234567890123456789011234567890234567890"
