
-- |
-- Module      :  Network.Ipfs.Tests.IpfsTest
-- Copyright   :  Alexander Krupenkin 2016-2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unknown
--
-- Module containing Ipfs command functions.
--

module Network.Ipfs.Tests.IpfsTests where

import  Data.Text            as TextS
import  Control.Exception() 
import  Test.Hspec

import Network.Ipfs.Api.Api  (KeyDetailsObj(..), KeyObj(..))
import Network.Ipfs.Api.Ipfs (keyGen, keyList, keyRm)

main :: IO ()
main = hspec $ do
  describe "keyGen" $ do
    it "should return the given key name in its response" $ do
      res <- keyGen (pack "TestA") (pack "rsa")
      (  case res of
        Left err -> (pack ("Error: " ++ show err))
        Right v -> keyName v ) `shouldBe` (pack "TestA")
          
    it "KeyDetailsObj returned by KeyGen should be present in the KeyObj's list returned returned by KeyList" $ do
      resGen <- keyGen (pack "TestB") (pack "rsa")
      resList <- keyList
      (case resList of
        Left _ -> [KeyDetailsObj (pack "Incorrect") ((pack "Incorrect"))]
        Right v -> keys v ) `shouldContain` (  case resGen of
                                                  Left _ -> [KeyDetailsObj (pack "More Incorrect") ((pack "MORE Incorrect"))]
                                                  Right v ->  [v] )

  describe "keyRm" $ do
    it "should return the given key name in its response" $ do
      res <- keyRm (pack "TestA")
      (  case res of
        Left err -> (pack ("Error: " ++ show err))
        Right v -> keyName $ Prelude.head $ keys v ) `shouldBe` (pack "TestA")

    it "KeyDetailsObj returned by KeyRm should not be present in the KeyObj's list returned returned by KeyList" $ do
      resRm <- keyRm (pack "TestB")
      resList <- keyList
      (case resList of
        Left _ -> [KeyDetailsObj (pack "Incorrect") ((pack "Incorrect"))]
        Right v -> keys v ) `shouldNotContain` (  case resRm of
                                                  Left _ -> [KeyDetailsObj (pack "More Incorrect") ((pack "MORE Incorrect"))]
                                                  Right v ->  keys v )