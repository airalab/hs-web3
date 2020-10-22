{-# LANGUAGE QuasiQuotes #-}

module Network.Ethereum.Test.MethodDumpSpec where

import           Network.Ethereum.Contract.TH
import           Test.Hspec

spec :: Spec
spec = parallel $
    describe "methodDump" $
        it "can dump an ABI" $
            let theApiDump = [abiFrom|tests/contracts/ERC20.json|]
             in theApiDump `shouldNotBe` ""
