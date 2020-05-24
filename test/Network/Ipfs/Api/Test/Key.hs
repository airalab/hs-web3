{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Ipfs.Api.Test.Key
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unknown
--
-- Key IPFS API testing module.
--

module Network.Ipfs.Api.Test.Key where

import           Control.Exception      ()
import           Control.Monad.Trans
import           Test.Hspec

import qualified Network.Ipfs.Api.Key   as Key
import           Network.Ipfs.Api.Types (KeyDetailsObj (..), KeyObj (..))
import           Network.Ipfs.Client    (runIpfs)

main :: IO ()
main = hspec $ do
  describe "keyGen" $ do
    it "should return the given key name in its response" $ runIpfs $ do
      res <- Key.gen "TestA" "rsa"
      liftIO $ keyName res `shouldBe` "TestA"

    it "KeyDetailsObj returned by KeyGen should be present in the KeyObj's list returned returned by KeyList" $
        runIpfs $ do
            resGen <- Key.gen "TestB" "rsa"
            resList <- Key.list
            liftIO $ keys resList `shouldContain` [resGen]

  describe "keyRm" $ do
    it "should return the given key name in its response" $ runIpfs $ do
      res <- Key.rm "TestA"
      liftIO $ (keyName $ Prelude.head $ keys res) `shouldBe` "TestA"

    it "KeyDetailsObj returned by KeyRm should not be present in the KeyObj's list returned returned by KeyList" $
        runIpfs $ do
            resRm <- Key.rm "TestB"
            resList <- Key.list
            liftIO $ keys resList `shouldNotContain` keys resRm
