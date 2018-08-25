{-# LANGUAGE OverloadedStrings #-}
module Network.Ethereum.ABI.Prim.Test.AddressSpec where

import           Data.ByteString            (ByteString)
import           Data.ByteString.Char8      (unpack)
import           Data.Foldable              (for_)
import           Data.Monoid                ((<>))
import           Data.Solidity.Prim.Address
import           Test.Hspec

spec :: Spec
spec = do
    describe "EIP55 Test Vectors" $ for_ checksummedAddrs (\addr ->
        it (unpack addr <> " should be checksummed") $ verifyChecksum addr `shouldBe` True)
    describe "EIP55 Test Vectors Tampered" $ for_ unchecksummedAddrs (\addr ->
        it (unpack addr <> " should not be checksummed") $ verifyChecksum addr `shouldBe` False)

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
