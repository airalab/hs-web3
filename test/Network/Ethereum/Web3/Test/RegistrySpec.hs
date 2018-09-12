{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}

module Network.Ethereum.Web3.Test.RegistrySpec where

import           Data.Default
import           Network.Ethereum.Contract.TH
import           Network.Ethereum.Web3
import           Network.Ethereum.Web3.Types


import           Test.Hspec

[abiFrom|test-support/build/contracts/abis/Registry.json|]

-- this spec is just to test compilation
spec :: Spec
spec = return ()


monitor :: Web3 ()
monitor = do
  let fltr1 = def :: Filter A
      fltr2 = def :: Filter B
  event fltr1 $ \_ -> pure TerminateEvent
  event fltr2 $ \_ -> pure TerminateEvent
  pure ()
