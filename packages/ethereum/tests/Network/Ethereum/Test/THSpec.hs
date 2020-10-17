{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
module Network.Ethereum.Test.THSpec where

import           Network.Ethereum.Contract.TH
import           Test.Hspec

-- 0x Exchange Contract that includes Tuples taken from:
-- https://raw.githubusercontent.com/0xProject/0x-monorepo/%400x/website%400.0.89/packages/contract-artifacts/artifacts/Exchange.json
[abiFrom|tests/contracts/Exchange.json|]

spec :: Spec
spec =
  describe "quasi-quoter" $
    it "can compile contract with tuples" $
      True `shouldBe` True
