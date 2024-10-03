{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}

-- |
-- Module      :  Network.Ethereum.Test.THSpec
-- Copyright   :  Aleksandr Krupenkin 2016-2024
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--

module Network.Ethereum.Test.THSpec where

import           Data.Tuple.OneTuple
import           Network.Ethereum.Contract.TH
import           Test.Hspec

-- 0x Exchange Contract that includes Tuples taken from:
-- https://raw.githubusercontent.com/0xProject/0x-monorepo/%400x/website%400.0.89/packages/contract-artifacts/artifacts/Exchange.json
[abiFrom|tests/contracts/Exchange.json|]

[abiFrom|tests/contracts/SingleField.json|]

spec :: Spec
spec = parallel $
  describe "quasi-quoter" $ do
    it "can compile contract with tuples" $
      True `shouldBe` True

    it "can compile single field structs" $ do
      let _ = SingleFieldFunctionData (OneTuple 123)
      True `shouldBe` True
