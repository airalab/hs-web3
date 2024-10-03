{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

-- |
-- Module      :  Network.Polkadot.Test.AccountSpec
-- Copyright   :  Aleksandr Krupenkin 2016-2024
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
--

module Network.Polkadot.Test.AccountSpec where

import           Data.ByteArray.HexString.TH (hex)
import           Test.Hspec

import           Network.Polkadot.Account
import           Network.Polkadot.Crypto

spec :: Spec
spec = parallel $ do
    let alice :: Ecdsa
        Right alice = from_seed [hex|0x8beef718111d62174809fc2b332c14471f6038404c5cee7b33ac2573ba60ed06|]
    let bob :: Ed25519
        Right bob = from_seed [hex|0xbd4e72a17a76b43ab59e7733cd0818d47c2e0ebcf88bfc5fb0192c8ee520c7d1|]

    describe "Public" $ do
        it "Ecdsa" $
            show (public alice) `shouldBe` "(2,0xc96289d7426111e7ec5cbb90d7d201ab0b3d7ab17166826ae4ff27cb0c6d3f23)"
        it "Ed25519" $
            show (public bob) `shouldBe` "0x8a016b9a1ca3709974ed7b1e1c79d6ed0f795899d212edc189cdb31318fec607"

    describe "Ss58Codec" $ do
        it "Ecdsa" $
            to_ss58check (into_account $ multi_signer alice)
                `shouldBe` "5EWfGfxVbLK2upe3Zfcqo9ZtALArwLgxkwcZaT3gMujmmFxU"
        it "Ed25519" $
            to_ss58check (into_account $ multi_signer bob)
                `shouldBe` "5FBetQhjRiJMfqqMU3f4cz8ho4majAPLzuZSdmxwxPtQTZ8V"
