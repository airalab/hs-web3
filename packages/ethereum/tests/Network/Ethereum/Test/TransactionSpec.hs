{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Ethereum.Test.TransactionSpec
-- Copyright   :  Aleksandr Krupenkin 2016-2024
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--

module Network.Ethereum.Test.TransactionSpec where

import           Crypto.Ecdsa.Utils           (importKey)
import           Crypto.Ethereum.Signature    (signTransaction)
import           Data.ByteArray.HexString     (HexString)
import           Network.Ethereum.Api.Types   (Call (..))
import           Network.Ethereum.Transaction (encodeTransaction)
import           Test.Hspec

spec :: Spec
spec = parallel $
    describe "Ethereum raw transactions" $
        it "can create and sign valid raw transaction" $ do
            -- using same example as in this blog post:
            -- https://medium.com/@codetractio/walkthrough-of-an-ethereum-improvement-proposal-eip-6fda3966d171
            let testCall = Call Nothing
                                (Just "0x3535353535353535353535353535353535353535")
                                (Just 21000)
                                (Just 20000000000)
                                (Just 1000000000000000000)
                                Nothing
                                (Just 9)
                key = "4646464646464646464646464646464646464646464646464646464646464646" :: HexString
                correctSignedTx = "f86c098504a817c800825208943535353535353535353535353535353535353535880de0b6b3a76400008025a028ef61340bd939bc2195fe537567866003e1a15d3c71ff63e1590620aa636276a067cbe9d8997f761aecb703304b3800ccf555c9f3dc64214b297fb1966a3b6d83"
            signTransaction (encodeTransaction testCall 1) (importKey key) `shouldBe` (correctSignedTx :: HexString)
