{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import           Control.Concurrent.MVar
import           Control.Monad.IO.Class           (liftIO)
import           Data.ByteArray                   (convert)
import           Data.ByteString                  (ByteString)
import           Data.Default
import           Data.Either                      (isRight)
import           Data.Maybe
import           Data.String                      (fromString)
import           Network.Ethereum.Contract.TH
import           Network.Ethereum.Web3            hiding (convert)
import qualified Network.Ethereum.Web3.Eth        as Eth
import           Network.Ethereum.Web3.Test.Utils
import           Network.Ethereum.Web3.Types      (Call (..), Filter (..))
import           System.Environment               (getEnv)
import           System.IO.Unsafe                 (unsafePerformIO)
import           Test.Hspec

[abiFrom|test-support/build/contracts/abis/ComplexStorage.json|]

spec :: Spec
spec = makeEnv `before` complexStorageSpec

complexStorageSpec :: SpecWith (ContractsEnv, Address)
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

        it "can set the values of a ComplexStorage and validate them with an event" $
          \(ContractsEnv _ contractAddress, primaryAccount) -> do
            contractAddress <- Prelude.fmap fromString . liftIO $ getEnv "COMPLEXSTORAGE_CONTRACT_ADDRESS"
            let theCall = callFromTo primaryAccount contractAddress
                fltr    = (def :: Filter ValsSet) { filterAddress = Just contractAddress }
            -- kick off listening for the ValsSet event
            vals <- newEmptyMVar
            fiber <- runWeb3Configured' $
                event fltr $ \vs -> do
                    liftIO $ putMVar vals vs
                    pure TerminateEvent
            -- kick off tx
            ret <- runWeb3Configured $ setValues theCall
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

        it "can verify that it set the values correctly" $ \(ContractsEnv _ contractAddress, primaryAccount) -> do
            contractAddress <- Prelude.fmap fromString . liftIO $ getEnv "COMPLEXSTORAGE_CONTRACT_ADDRESS"
            let theCall = callFromTo primaryAccount contractAddress
                runGetterCall f = runWeb3Configured (f theCall)
            -- there really has to be a better way to do this
            uintVal'    <- runGetterCall uintVal
            intVal'     <- runGetterCall intVal
            boolVal'    <- runGetterCall boolVal
            int224Val'  <- runGetterCall int224Val
            boolsVal    <- runGetterCall $ \c -> boolVectorVal c 0
            intsVal     <- runGetterCall $ \c -> intListVal c 0
            stringVal'  <- runGetterCall stringVal
            bytes16Val' <- runGetterCall bytes16Val
            bytes2s     <- runGetterCall $ \c -> bytes2VectorListVal c 0 0
            uintVal'    `shouldBe` sUint
            intVal'     `shouldBe` sInt
            boolVal'    `shouldBe` sBool
            int224Val'  `shouldBe` sInt224
            boolsVal    `shouldBe` True
            intsVal     `shouldBe` sInts  Prelude.!! 0
            stringVal'  `shouldBe` sString
            bytes16Val' `shouldBe` sBytes16
            bytes2s `shouldBe` sByte2sElem

        it "can decode a complicated value correctly" $ \(ContractsEnv _ contractAddress, primaryAccount) -> do
            contractAddress <- Prelude.fmap fromString . liftIO $ getEnv "COMPLEXSTORAGE_CONTRACT_ADDRESS"
            let theCall = callFromTo primaryAccount contractAddress
                runGetterCall f = runWeb3Configured (f theCall)
            allVals <- runGetterCall getVals
            allVals `shouldBe` (sUint, sInt, sBool, sInt224, sBools, sInts, sString, sBytes16, sByte2s)
