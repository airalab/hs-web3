{-# LANGUAGE OverloadedStrings #-}
module Crypto.Ethereum.Test.EcdsaSpec where

import           Crypto.Secp256k1           (SecKey, derivePubKey)
import           Data.ByteArray             (convert)
import           Data.ByteString            (ByteString, pack)
import           Data.Serialize             (decode)
import           Test.Hspec
import           Test.Hspec.QuickCheck

import           Crypto.Ethereum
import           Data.ByteArray.HexString   (HexString)
import           Data.Solidity.Prim.Address (fromPubKey)

spec :: Spec
spec = do
    describe "Ethereum signatures" $ do
        let key = "0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20" :: SecKey
            address = fromPubKey (derivePubKey key)
            message = "Hello World!" :: ByteString
            sign = "0xd4a9620cd94387a31b9333935f1e76fbee8467c283d07c39c1606dc4e2af021e317293af09601dbb48f547e40f6b98fe8a67a23dcd1f7f8d054695a81521177001" :: HexString
            sign' = either error id $ decode (convert sign)

        it "sign message by Ethereum private key" $ do
            ecsign key message `shouldBe` sign'

        it "verify message by Ethereum public key" $ do
            ecrecover sign' message `shouldBe` Just address

        prop "round robin sign/verify for random message" $ \chars ->
            let msg = pack chars
             in ecrecover (ecsign key msg) msg `shouldBe` Just address
