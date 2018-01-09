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
import           Data.Foldable                    (forM_)
import           Data.List                        (sort)
import           Data.Maybe
import           Data.Monoid
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
import           Network.Ethereum.Web3.Types
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
    it "can stream events starting and ending in the future, unbounded" $ \primaryAccount -> do
        var <- newMVar []
        let theCall = callFromTo primaryAccount contractAddress
            theSets = map (fromJust . uIntNFromInteger) [1, 2, 3]
        print "Setting up the filter..."
        fiber <- runWeb3Configured' $ do
          let fltr = eventFilter (Proxy :: Proxy CountSet) contractAddress
          forkWeb3 $ event fltr $ \(CountSet cs) -> do
            v <- liftIO $ takeMVar var
            let newV = cs : v
            liftIO $ putMVar var newV
            if length newV == 3
                then return TerminateEvent
                else return ContinueEvent
        print "Setting the values..."
        setValues theCall theSets
        wait fiber
        print "Filter caught 3 values..."
        vals <- takeMVar var
        sort vals `shouldBe` sort theSets

    it "can stream events starting and ending in the future, bounded" $ \primaryAccount -> do
        runWeb3Configured Eth.blockNumber >>= \bn -> awaitBlock (bn + 1)
        var <- newMVar []
        let theCall = callFromTo primaryAccount contractAddress
            theSets = map (fromJust . uIntNFromInteger) [13, 14, 15]
        start <- runWeb3Configured Eth.blockNumber
        let later = BlockWithNumber (start + 3)
            latest = BlockWithNumber (start + 8)
            fltr = (eventFilter (Proxy :: Proxy CountSet) contractAddress) { filterFromBlock = later
                                                                           , filterToBlock = latest
                                                                           }
        print "Setting up the filter..."
        fiber <- runWeb3Configured' $
          forkWeb3 $ event fltr $ \(CountSet cs) -> do
            v <- liftIO $ takeMVar var
            let newV = cs : v
            liftIO $ putMVar var newV
            if length newV == 3
                then return TerminateEvent
                else return ContinueEvent
        awaitBlock (start + 3)
        print "Setting the values..."
        setValues theCall theSets
        wait fiber
        print "Filter caught 3 values..."
        vals <- takeMVar var
        sort vals `shouldBe` sort theSets

    it "can stream events starting in the past and ending in the future" $ \primaryAccount -> do
        runWeb3Configured Eth.blockNumber >>= \bn -> awaitBlock (bn + 1)
        var <- newMVar []
        blockNumberVar <- newEmptyMVar
        let theCall = callFromTo primaryAccount contractAddress
            theSets1 = map (fromJust . uIntNFromInteger) [7, 8, 9]
            theSets2 = map (fromJust . uIntNFromInteger) [10, 11, 12]
        start <- runWeb3Configured Eth.blockNumber
        let fltr = eventFilter (Proxy :: Proxy CountSet) contractAddress
        fiber <- runWeb3Configured' $ do
          forkWeb3 $ event fltr $ \(CountSet cs) -> do
            v <- liftIO $ takeMVar var
            let newV = cs : v
            liftIO $ putMVar var newV
            if length newV == 3
                then do
                    change <- ask
                    liftIO $ putMVar blockNumberVar (changeBlockNumber change)
                    return TerminateEvent
                else do
                  return ContinueEvent
        print "Running first transactions as past transactions..."
        setValues theCall theSets1
        wait fiber
        print "All past transactions succeeded... "
        end <- takeMVar blockNumberVar
        awaitBlock $ end + 1 -- make past transactions definitively in past
        var' <- newMVar []
        fiber <- runWeb3Configured' $ do
          let fltr = (eventFilter (Proxy :: Proxy CountSet) contractAddress) {filterFromBlock = BlockWithNumber start}
          forkWeb3 $ event fltr $ \(CountSet cs) -> do
            v <- liftIO $ takeMVar var'
            let newV = cs : v
            liftIO $ putMVar var' newV
            if length newV == 6
                then return TerminateEvent
                else return ContinueEvent
        print "Setting more values"
        setValues theCall theSets2
        wait fiber
        print "All new values have ben set"
        vals <- takeMVar var'
        sort vals `shouldBe` sort (theSets1 <> theSets2)

    it "can stream events starting and ending in the past, bounded" $ \primaryAccount -> do
        runWeb3Configured Eth.blockNumber >>= \bn -> awaitBlock (bn + 1)
        var <- newMVar []
        let theCall = callFromTo primaryAccount contractAddress
            theSets = map (fromJust . uIntNFromInteger) [4, 5, 6]
        start <- runWeb3Configured Eth.blockNumber
        blockNumberVar <- newEmptyMVar
        let fltr = eventFilter (Proxy :: Proxy CountSet) contractAddress
        print "Setting up filter for past transactions..."
        fiber <- runWeb3Configured' $ do
          forkWeb3 $ event fltr $ \(CountSet cs) -> do
            v <- liftIO $ takeMVar var
            let newV = cs : v
            liftIO $ putMVar var newV
            if length newV == 3
                then do
                    change <- ask
                    liftIO $ putMVar blockNumberVar (changeBlockNumber change)
                    return TerminateEvent
                else do
                  return ContinueEvent
        print "Setting values"
        setValues theCall theSets
        wait fiber
        print "All values have been set"
        end <- takeMVar blockNumberVar
        var' <- newMVar []
        let fltr' = fltr { filterFromBlock = BlockWithNumber start
                         , filterToBlock = BlockWithNumber end
                         }
        awaitBlock $ end + 1 -- make it definitively in the past
        runWeb3Configured $
          event fltr' $ \(CountSet cs) -> do
            v <- liftIO $ takeMVar var'
            let newV = cs : v
            liftIO $ putMVar var' newV
            if length newV == 3
                then return TerminateEvent
                else return ContinueEvent
        vals <- takeMVar var'
        sort vals `shouldBe` sort theSets

setValues :: Call -> [UIntN 256] -> IO ()
setValues theCall theSets = forM_ theSets $ \v -> do
  runWeb3Configured (setCount theCall v)
  threadDelay 1000000
