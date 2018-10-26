{-# LANGUAGE OverloadedStrings #-}

module Network.Ethereum.Web3.Test.LocalAccountSpec where

import           Crypto.Ethereum                     (SecKey)
import           Test.Hspec

import           Network.Ethereum.Account.PrivateKey (signTransaction)
import           Network.Ethereum.Api.Types          (Call (..), Quantity (..))

-- using same example as in this blog post:
-- https://medium.com/@codetractio/walkthrough-of-an-ethereum-improvement-proposal-eip-6fda3966d171
spec :: Spec
spec = describe "transaction signing" $ do
    let testCall = Call Nothing
                        (Just "0x3535353535353535353535353535353535353535")
                        (Just . Quantity $ 21000)
                        (Just . Quantity $ 20000000000)
                        (Just . Quantity $ 1000000000000000000)
                        Nothing
                        (Just 9)
        privKey = "4646464646464646464646464646464646464646464646464646464646464646" :: SecKey
        correctSignedTx = "0xf86c098504a817c800825208943535353535353535353535353535353535353535880de0b6b3a76400008025a028ef61340bd939bc2195fe537567866003e1a15d3c71ff63e1590620aa636276a067cbe9d8997f761aecb703304b3800ccf555c9f3dc64214b297fb1966a3b6d83"

    it "can create valid raw transaction" $
        signTransaction testCall 1 privKey `shouldBe` correctSignedTx
