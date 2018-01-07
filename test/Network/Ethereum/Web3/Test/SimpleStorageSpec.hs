{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}


-- Module      :  Network.Ethereum.Web3.Test.SimpleStorage
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- SimpleStorage is a Solidity contract which stores a uint256.
-- The point of this test is to test function calls to update and
-- read the value, as well as an event monitor.

module Network.Ethereum.Web3.Test.SimpleStorageSpec where

import           Control.Concurrent               (threadDelay)
import           Control.Concurrent.Async         (wait)
import           Control.Concurrent.MVar
import           Control.Monad                    (void)
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Trans.Reader       (ask)
import           Data.ByteString                  (ByteString)
import           Data.Default
import           Data.Either                      (isRight)
import           Data.List                        (sort)
import           Data.Proxy
import           Data.String                      (fromString)
import qualified Data.Text                        as T
import           Data.Traversable                 (for)
import           GHC.TypeLits
import           Network.Ethereum.Web3            hiding (convert)
import           Network.Ethereum.Web3.Contract   (Event (..))
import qualified Network.Ethereum.Web3.Eth        as Eth
import           Network.Ethereum.Web3.Test.Utils
import           Network.Ethereum.Web3.TH
import           Network.Ethereum.Web3.Types      (Call (..), Change (..), Filter (..), BlockNumber)
import           Numeric                          (showHex)
import           System.Environment               (getEnv)
import           System.IO.Unsafe                 (unsafePerformIO)
import           Test.Hspec

[abiFrom|test-support/build/contracts/abis/SimpleStorage.json|]

contractAddress :: Address
contractAddress = fromString . unsafePerformIO $ getEnv "SIMPLESTORAGE_CONTRACT_ADDRESS"

spec :: Spec
spec = describe "Simple Storage" $ do
    it "should inject contract addresses" injectExportedEnvironmentVariables
    withPrimaryEthereumAccount `before` interactions
    withPrimaryEthereumAccount `before` events

interactions :: SpecWith Address
interactions = describe "can interact with a SimpleStorage contract" $ do
    -- todo: this should ideally be arbitrary!
    let theValue = 12345
    it "can set the value of a SimpleStorage contract" $ \primaryAccount -> do
        let theCall = callFromTo primaryAccount contractAddress
        ret <- runWeb3Configured $ setCount theCall theValue
        True `shouldBe` True -- we need to get this far

    it "can read the value back" $ \primaryAccount -> do
        let theCall = callFromTo primaryAccount contractAddress
        now <- runWeb3Configured Eth.blockNumber
        let later = now + 3
        awaitBlock later
        v <- runWeb3Configured (count theCall)
        v `shouldBe` theValue

events :: SpecWith Address
events = describe "can interact with a SimpleStorage contract across block intervals" $ do
    let basicCountFilter = eventFilter (Proxy :: Proxy CountSet) contractAddress
    it "can stream events starting and ending in the future, unbounded" $ \primaryAccount -> do
        var <- newMVar []
        let theCall = callFromTo primaryAccount contractAddress
            theSets = [8, 9, 10]
        _ <- runWeb3Configured $ do
          liftIO $ print "Launching event monitor ..."
          fiber <- event basicCountFilter $ \(CountSet cs) -> do
            liftIO . print $ "Got count: " ++ show cs
            v <- liftIO $ takeMVar var
            let newV = cs : v
            liftIO $ putMVar var newV
            if length newV == 3
                then do
                  liftIO $ print "Received All Values"
                  return TerminateEvent
                else return ContinueEvent
          liftIO $ print "Setting values on SimpleStorage ..."
          _ <- forkWeb3 $ setValues theCall theSets
          liftIO $ do
            print "Waiting for event monitor to complete ..."
            wait fiber
        vals <- takeMVar var
        sort vals `shouldBe` sort theSets

--    it "can stream events starting and ending in the future, bounded" $ \primaryAccount -> do
--        var <- newMVar []
--        let theCall = callFromTo primaryAccount contractAddress
--            theSets = [8, 9, 10]
--        now <- runWeb3Configured Eth.blockNumber
--        print $ "Current blockNumber is: " ++ show now
--        let later = now + 3
--            latest = now + 8
--            filter = basicCountFilter {filterFromBlock = later, filterToBlock = latest}
--        fiber <- runWeb3Configured $ do
--          event filter $ \e@(CountSet cs) -> do
--            liftEff $ log $ "Received Event: " <> show e
--            old <- liftIO $ takeMVar var
--            let new = count cs : old
--            if length new == 3
--               then return TerminateEvent
--               else do
--                 liftIO $ putMVar new var
--                 reurn $ ContinueEvent
--        runWeb3Configured $ hangOutTillBlock later
--        setValues theSets
--        _ <- wait fiber
--        vals <- takeMVar var
--        sort vals `shouldBe` sort theSets
--
--hangOutTillBlock :: Provider p => BlockNumber -> Web3 p ()
--hangOutTillBlock bn = do
--  bn' <- Eth.blockNumber
--  if bn' >= bn
--    then return ()
--    else liftIO (threadDelay 1000000) *> hangOutTillBlock bn

setValues :: Provider p => Call -> [UIntN 256] -> Web3 p ()
setValues theCall theSets = void $ for theSets $ \v -> do
  liftIO $ threadDelay 2000000
  setCount theCall v
