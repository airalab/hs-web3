{-# LANGUAGE OverloadedStrings #-}
module Data.Solidity.Test.AddressSpec where

import           Crypto.Secp256k1           (SecKey, derivePubKey)
import           Data.ByteString            (ByteString)
import           Data.ByteString.Char8      (unpack)
import           Data.Foldable              (for_)
import           Data.Monoid                ((<>))
import           Test.Hspec

import           Data.Solidity.Prim.Address

spec :: Spec
spec = do
    describe "EIP55 Test Vectors" $ for_ checksummedAddrs (\addr ->
        it (unpack addr <> " should be checksummed") $ verifyChecksum addr `shouldBe` True)

    describe "EIP55 Test Vectors Tampered" $ for_ unchecksummedAddrs (\addr ->
        it (unpack addr <> " should not be checksummed") $ verifyChecksum addr `shouldBe` False)

    describe "Conversion from/to hex string" $ do
        it "should convert from/to on valid hex" $ do
            fromHexString "0x6370eF2f4Db3611D657b90667De398a2Cc2a370C"
                `shouldBe` Right "0x6370eF2f4Db3611D657b90667De398a2Cc2a370C"

            toHexString "0x6370eF2f4Db3611D657b90667De398a2Cc2a370C"
                `shouldBe` "0x6370eF2f4Db3611D657b90667De398a2Cc2a370C"

        it "should fail on broken hex" $ do
            fromHexString "0x42"
                `shouldBe` Left "Incorrect address length: 1"


    describe "Conversion from Secp256k1 keys" $ do
        it "derivation from private key" $ do
            let key = "0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20" :: SecKey
            fromPubKey (derivePubKey key)
                `shouldBe` "0x6370eF2f4Db3611D657b90667De398a2Cc2a370C"

checksummedAddrs :: [ByteString]
checksummedAddrs =
    [ "0x52908400098527886E0F7030069857D2E4169EE7"
    , "0x8617E340B3D01FA5F11F306F4090FD50E238070D"
    , "0xde709f2102306220921060314715629080e2fb77"
    , "0x27b1fdb04752bbc536007a920d24acb045561c26"
    , "0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAed"
    , "0xfB6916095ca1df60bB79Ce92cE3Ea74c37c5d359"
    , "0xdbF03B407c01E7cD3CBea99509d93f8DDDC8C6FB"
    , "0xD1220A0cf47c7B9Be7A2E6BA89F429762e7b9aDb"
    ]

unchecksummedAddrs :: [ByteString]
unchecksummedAddrs =
    [ "0x52908400098527886E0F7030069857D2E4169Ee7"
    , "0x8617E340B3D01FA5F11F306F4090FD50E238070d"
    , "0xde709f2102306220921060314715629080e2fB77"
    , "0x27b1fdb04752bbc536007a920d24acb045561C26"
    , "0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAeD"
    , "0xfB6916095ca1df60bB79Ce92cE3Ea74c37c5D359"
    , "0xdbF03B407c01E7cD3CBea99509d93f8DDDC8C6Fb"
    , "0xD1220A0cf47c7B9Be7A2E6BA89F429762e7b9aDB"
    ]
