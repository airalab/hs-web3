{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Crypto.Random.Test.HmacDrbgSpec
-- Copyright   :  Aleksandr Krupenkin 2016-2024
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- Inspired by https://github.com/indutny/hmac-drbg
--

module Crypto.Random.Test.HmacDrbgSpec where

import           Crypto.Hash.Algorithms   (SHA256)
import           Crypto.Random            (randomBytesGenerate)
import           Data.ByteString          (ByteString)
import           Test.Hspec

import           Crypto.Random.HmacDrbg
import           Data.ByteArray.HexString (HexString)

spec :: Spec
spec = parallel $ do
    describe "HMAC-DRBG-SHA256" $ do
        it "indutny/hmac-drbg test vectors" $ do
            let doDrbg :: ByteString -> HexString
                doDrbg seed = fst (randomBytesGenerate 32 (initialize seed) :: (HexString, HmacDrbg SHA256))

            doDrbg "totally random0123456789secret noncemy drbg"
                `shouldBe` "018ec5f8e08c41e5ac974eb129ac297c5388ee1864324fa13d9b15cf98d9a157"

            doDrbg "totally random0123456789secret nonce"
                `shouldBe` "ed5d61ecf0ef38258e62f03bbb49f19f2cd07ba5145a840d83b134d5963b3633"
