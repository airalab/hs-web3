{-# LANGUAGE DataKinds #-}

-- |
-- Module      :  Data.Solidity.Test.IntSpec
-- Copyright   :  Aleksandr Krupenkin 2016-2024
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--

module Data.Solidity.Test.IntSpec where

import           Test.Hspec

import           Data.Solidity.Prim.Int

spec :: Spec
spec = parallel $ do
    describe "Unsigned integer overflow" $ do
        it "UIntN 256" $ do
            (negate 1 :: UIntN 256) `shouldBe` 115792089237316195423570985008687907853269984665640564039457584007913129639935
            (maxBound + 1 :: UIntN 256) `shouldBe` 0

        it "UIntN 128" $ do
            (negate 1 :: UIntN 128) `shouldBe` 340282366920938463463374607431768211455
            (maxBound + 1 :: UIntN 128) `shouldBe` 0

        it "UIntN 64" $ do
            (negate 1 :: UIntN 64) `shouldBe` 18446744073709551615
            (maxBound + 1 :: UIntN 64) `shouldBe` 0

        it "UIntN 32" $ do
            (negate 1 :: UIntN 32) `shouldBe` 4294967295
            (maxBound + 1 :: UIntN 32) `shouldBe` 0

        it "UIntN 16" $ do
            (negate 1 :: UIntN 16) `shouldBe` 65535
            (maxBound + 1 :: UIntN 16) `shouldBe` 0

        it "UIntN 8" $ do
            (negate 1 :: UIntN 8)        `shouldBe` 255
            (maxBound + 1 :: UIntN 8) `shouldBe` 0
