{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}

module Network.Ethereum.Web3.Test.ERC20Spec where

import           Data.Default
import           Network.Ethereum.Contract.TH
import           Network.Ethereum.Web3
import           Network.Ethereum.Web3.Types


import           Test.Hspec

[abiFrom|test-support/abis/ERC20.json|]

spec :: Spec
spec = return ()


getBalance :: Web3 (UIntN 256)
getBalance = balanceOf def Latest ("0x1234567890123456789011234567890234567890" :: Address)
