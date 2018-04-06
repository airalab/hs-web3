{-# LANGUAGE QuasiQuotes #-}

module Network.Ethereum.Web3.Test.MethodDumpSpec where

import           Network.Ethereum.Contract.TH
import           Test.Hspec
import           Text.Printf


spec :: Spec
spec = describe "methodDump" $
    it "can dump an ABI" $  do
        let theApiDump = [abiFrom|data/ERC20.json|]
         in theApiDump `shouldNotBe` ""
