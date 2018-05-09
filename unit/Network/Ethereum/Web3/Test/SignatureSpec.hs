{-# LANGUAGE OverloadedStrings #-}

module Network.Ethereum.Web3.Test.SignatureSpec where

import           Data.ByteArray               (Bytes, convert)
import qualified Data.ByteString.Base16       as BS16
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import           Network.Ethereum.Web3.Types
import           Network.Ethereum.Web3.Transaction
import           Test.Hspec


-- using same example as in this blog post:
-- https://medium.com/@codetractio/walkthrough-of-an-ethereum-improvement-proposal-eip-6fda3966d171
spec :: Spec
spec = describe "transaction signing" $
    it "can create valid raw transaction" $ do
        let rawTx = T.decodeUtf8 . BS16.encode . convert <$> createRawTransaction testCall 1 privKey
        rawTx `shouldBe` (Just correctSignedTx)
    where testCall = Call Nothing
                          (Just "0x3535353535353535353535353535353535353535")
                          (Just . Quantity $ 21000)
                          (Just . Quantity $ 20000000000)
                          (Just . Quantity $ 1000000000000000000)
                          Nothing
                          (Just 9)
          privKey = fst . BS16.decode . T.encodeUtf8 $ "4646464646464646464646464646464646464646464646464646464646464646"
          correctSignedTx = "f86c098504a817c800825208943535353535353535353535353535353535353535880de0b6b3a76400008025a028ef61340bd939bc2195fe537567866003e1a15d3c71ff63e1590620aa636276a067cbe9d8997f761aecb703304b3800ccf555c9f3dc64214b297fb1966a3b6d83"
