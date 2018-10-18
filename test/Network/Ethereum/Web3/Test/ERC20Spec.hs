{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
module Network.Ethereum.Web3.Test.ERC20Spec where

import           Test.Hspec

import           Network.Ethereum.Contract.TH (abiFrom)
import           Network.Ethereum.Web3        (Account, UIntN)
import           Network.JsonRpc.TinyClient   (JsonRpcM)

[abiFrom|examples/token/ERC20.json|]

-- this spec is just to test compilation
spec :: Spec
spec = return ()

getBalance :: (JsonRpcM m, Account p t, Functor (t m))
           => t m (UIntN 256)
getBalance = balanceOf "0x1234567890123456789011234567890234567890"
