{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
module Network.Ethereum.Contract.Test.THSpec where


import Test.Hspec
import           Network.Ethereum.Contract.TH


-- 0x Exchange Contract that includes Tuples taken from:
-- https://raw.githubusercontent.com/0xProject/0x-monorepo/%400x/website%400.0.89/packages/contract-artifacts/artifacts/Exchange.json
[abiFrom|test/contracts/Exchange.json|]

spec :: Spec
spec = 
  describe "quasi-quoter" $ 
    it "can compile contract with tuples" $ 
      True `shouldBe` True
    


