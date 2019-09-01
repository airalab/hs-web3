
-- |
-- Module      :  Network.Ipfs.Api.Test.Key
-- Copyright   :  Alexander Krupenkin 2016-2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unknown
--
-- Module containing Ipfs command functions.
--

module Network.Ipfs.Api.Test.Key where

import  Data.Text              as TextS
import  Control.Exception() 
import  Control.Monad.Trans
import  Test.Hspec

import  Network.Ipfs.Api.Api   (KeyDetailsObj(..), KeyObj(..))
import  Network.Ipfs.Api.Ipfs  (keyGen, keyList, keyRm,runIpfs)

main :: IO ()
main = hspec $ do
  describe "keyGen" $ do
    it "should return the given key name in its response" $ runIpfs $ do
      res <- keyGen (pack "TestA") (pack "rsa")
      liftIO $ ( keyName res ) `shouldBe` (pack "TestA")
          
    it "KeyDetailsObj returned by KeyGen should be present in the KeyObj's list returned returned by KeyList" $ runIpfs $ do
      resGen <- keyGen (pack "TestB") (pack "rsa")
      resList <- keyList
      liftIO $ (keys resList) `shouldContain` ( [resGen] )

  describe "keyRm" $ do
    it "should return the given key name in its response" $ runIpfs $ do
      res <- keyRm (pack "TestA")
      liftIO $ ( keyName $ Prelude.head $ keys res ) `shouldBe` (pack "TestA")

    it "KeyDetailsObj returned by KeyRm should not be present in the KeyObj's list returned returned by KeyList" $ runIpfs $ do
      resRm <- keyRm (pack "TestB")
      resList <- keyList
      liftIO $ (keys resList) `shouldNotContain` (keys resRm)