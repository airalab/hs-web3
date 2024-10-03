{-# LANGUAGE QuasiQuotes #-}

-- |
-- Module      :  Network.Ethereum.Test.MethodDumpSpec
-- Copyright   :  Aleksandr Krupenkin 2016-2024
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--

module Network.Ethereum.Test.MethodDumpSpec where

import           Network.Ethereum.Contract.TH
import           Test.Hspec

spec :: Spec
spec = parallel $
    describe "methodDump" $
        it "can dump an ABI" $
            let theApiDump = [abiFrom|tests/contracts/ERC20.json|]
             in theApiDump `shouldNotBe` ""
