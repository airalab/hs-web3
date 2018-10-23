{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}

-- |
-- Module      :  Network.Ethereum.Web3.Test.ComplexStorage
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- ComplexStorage is a Solidity contract which has global variables of
-- several different types. The point of this test is to test the encoding
-- of a complicated Solidity tuple, consisting of dynamically and statically
-- sized components.
--

module Network.Ethereum.Web3.Test.ComplexStorageSpec where

import           Control.Concurrent.Async         (wait)
import           Control.Concurrent.MVar          (newEmptyMVar, putMVar,
                                                   takeMVar)
import           Control.Monad.IO.Class           (liftIO)
import           Data.Default                     (def)

import           Network.Ethereum.Api.Types       (Filter (..))
import           Network.Ethereum.Contract        (new)
import           Network.Ethereum.Contract.TH
import           Network.Ethereum.Web3            hiding (convert)
import           Network.Ethereum.Web3.Test.Utils


import           Test.Hspec

[abiFrom|test/contracts/ComplexStorage.json|]

deploy :: IO Address
deploy = do
    Just address <- web3 $ withAccount () $ withParam id $ new ComplexStorageContract
    putStrLn $ "ComplexStorage: " ++ show address
    return address

spec :: Spec
spec = deploy `before` complexStorageSpec

complexStorageSpec :: SpecWith Address
complexStorageSpec = do
  describe "can interact with a ComplexStorage contract" $ do
        -- todo: these should ideally be arbitrary!
        let sUint   = 1
            sInt    = -1
            sBool   = True
            sInt224 = 221
            sBools   = [True, False]
            sInts    = [1, 1, -3]
            sString  = "hello"
            sBytes16 = "\x12\x34\x56\x78\x12\x34\x56\x78\x12\x34\x56\x78\x12\x34\x56\x78"
            sByte2sElem = "\x12\x34"
            sByte2sVec = [sByte2sElem, sByte2sElem, sByte2sElem, sByte2sElem]
            sByte2s = [sByte2sVec, sByte2sVec]

        it "can set the values of a ComplexStorage and validate them with an event" $ \storage -> do
            let fltr = (def :: Filter ValsSet) { filterAddress = Just [storage] }
            -- kick off listening for the ValsSet event
            vals <- newEmptyMVar
            fiber <- web3 $
                event fltr $ \vs -> do
                    liftIO $ putMVar vals vs
                    pure TerminateEvent
            -- kick off tx
            _ <- contract storage $ setValues
                                    sUint
                                    sInt
                                    sBool
                                    sInt224
                                    sBools
                                    sInts
                                    sString
                                    sBytes16
                                    sByte2s
            -- wait for its ValsSet event
            wait fiber
            (ValsSet vsA vsB vsC vsD vsE vsF vsG vsH vsI) <- takeMVar vals
            vsA `shouldBe` sUint
            vsB `shouldBe` sInt
            vsC `shouldBe` sBool
            vsD `shouldBe` sInt224
            vsE `shouldBe` sBools
            vsF `shouldBe` sInts
            vsG `shouldBe` sString
            vsH `shouldBe` sBytes16
            vsI `shouldBe` sByte2s

        it "can verify that it set the values correctly" $ \storage -> do
            -- Write a values
            _ <- contract storage $ setValues
                                    sUint
                                    sInt
                                    sBool
                                    sInt224
                                    sBools
                                    sInts
                                    sString
                                    sBytes16
                                    sByte2s
            -- Read a couple of values
            (uintVal', intVal', boolVal', int224Val', boolsVal, intsVal, stringVal', bytes16Val', bytes2s)
                <- contract storage $ (,,,,,,,,)
                    <$> uintVal
                    <*> intVal
                    <*> boolVal
                    <*> int224Val
                    <*> boolVectorVal 0
                    <*> intListVal 0
                    <*> stringVal
                    <*> bytes16Val
                    <*> bytes2VectorListVal 0 0

            uintVal'    `shouldBe` sUint
            intVal'     `shouldBe` sInt
            boolVal'    `shouldBe` sBool
            int224Val'  `shouldBe` sInt224
            boolsVal    `shouldBe` True
            intsVal     `shouldBe` head sInts
            stringVal'  `shouldBe` sString
            bytes16Val' `shouldBe` sBytes16
            bytes2s     `shouldBe` sByte2sElem

        it "can decode a complicated value correctly" $ \storage -> do
            -- Write a values
            _ <- contract storage $ setValues
                                    sUint
                                    sInt
                                    sBool
                                    sInt224
                                    sBools
                                    sInts
                                    sString
                                    sBytes16
                                    sByte2s
            -- Read a all values
            allVals <- contract storage getVals
            allVals `shouldBe` (sUint, sInt, sBool, sInt224, sBools, sInts, sString, sBytes16, sByte2s)
