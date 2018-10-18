{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}

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

module Network.Ethereum.Web3.Test.LinearizationSpec where

import           Control.Concurrent               (forkIO)
import           Control.Concurrent.Async         (Async, async, wait)
import           Control.Concurrent.MVar
import           Control.Concurrent.STM           (atomically)
import           Control.Concurrent.STM.TQueue    (TQueue, flushTQueue,
                                                   newTQueueIO, writeTQueue)
import           Control.Concurrent.STM.TSem      (TSem, newTSem, signalTSem,
                                                   waitTSem)
import           Control.Monad                    (forM, void)
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Control.Monad.Trans.Reader       (ReaderT, ask)
import           Data.Default
import           Data.Either
import           Data.List                        (sort)
import           Data.Maybe                       (fromJust)
import           System.Environment               (getEnv)
import           System.Random                    (randomRIO)
import           Test.Hspec

import qualified Network.Ethereum.Api.Eth         as Eth
import           Network.Ethereum.Api.Types       (Change (..), Filter (..),
                                                   TxReceipt, unQuantity)
import           Network.Ethereum.Contract        (new)
import           Network.Ethereum.Contract.Event
import           Network.Ethereum.Contract.TH     (abiFrom)
import           Network.Ethereum.Web3
import           Network.Ethereum.Web3.Test.Utils

[abiFrom|test/contracts/Linearization.json|]

deploy :: IO Address
deploy = do
    Just address <- web3 $ withAccount () $ withParam id $ new LinearizationContract
    putStrLn $ "Linearization: " ++ show address
    return address

spec :: Spec
spec = do
    deploy `before` linearizationSpec
    deploy `before` floodSpec

floodCount :: Int
floodCount = 200

-- waitTSem will block until the counter is positive (i.e., > 0)
-- so if there's -(floodCount - 1), that means when the floodCount `signalTSem`s are done
-- there will be 1 unit left in the TSem for the waitTSem at the end of a test
floodSemCount :: Int
floodSemCount = -(floodCount - 1)

linearizationSpec :: SpecWith Address
linearizationSpec =
    describe "can bundle and linearize events" $ do
        it "can call e12" $ \linearization -> do
            var <- monitorE1OrE2 linearization
            _ <- contract linearization e12
            res <- takeMVar var
            res `shouldSatisfy` isLeft

        it "can call e21" $ \linearization -> do
            -- wait on the next block
            web3 Eth.blockNumber >>= \bn -> awaitBlock (bn + 1)
            var <- monitorE1OrE2 linearization
            _ <- contract linearization e21
            res <- takeMVar var
            res `shouldSatisfy` isRight

singleFlood :: forall m. (MonadIO m) => Address -> m TxReceipt
singleFlood linearization = liftIO $ do
    rando :: Int <- randomRIO (1, 4)
    contract linearization $
        case rando of
            1 -> e1
            2 -> e2
            3 -> e3
            4 -> e4
            _ -> error "got a number outside of (1,4) after randomR (1,4)"

floodSpec :: SpecWith Address
floodSpec = describe "can correctly demonstrate the difference between `multiEvent` and multiple `event'`s" $ do
    it "properly linearizes with `multiEvent` when flooded and doesn't with multiple `event`s" $ \linearization -> do
        multiQ <- monitorAllFourMulti linearization
        parQ <- monitorAllFourPar linearization
        -- to let the filter settle so we dont block indefinitely on missing events?
        sleepBlocks 10

        -- flood em and wait for all to finish
        void . forM [1..floodCount] . const . liftIO $ singleFlood linearization
        -- to let the event listeners catch up
        sleepBlocks 10

        -- wait for all multiEvents to be received and flush em out
        multiReceivedEvents <- liftIO . atomically $ flushTQueue multiQ
        parReceivedEvents <- liftIO . atomically $ flushTQueue parQ

        -- did we get at least 1/4 of the events? (this is a gotcha when flooding, sometimes nonces get repeated)
        length multiReceivedEvents `shouldSatisfy` (>= (floodCount `div` 4))
        length parReceivedEvents `shouldSatisfy` (>= (floodCount `div` 4))

        -- did both listeners see the same amount of events?
        length multiReceivedEvents `shouldBe` length parReceivedEvents

        -- the events pushed into the multi TQueue should already be sorted if they happened in the right order
        sort multiReceivedEvents `shouldBe` multiReceivedEvents
        -- the events pushed into the TQueue should not be sorted if they didnt come in in the right order
        sort parReceivedEvents `shouldNotBe` parReceivedEvents

monitorE1OrE2 :: Address -> IO (MVar (Either E1 E2))
monitorE1OrE2 addr = do
    var <- newEmptyMVar
    let fltr1 = (def :: Filter E1) { filterAddress = Just [addr] }
        fltr2 = (def :: Filter E2) { filterAddress = Just [addr] }
        filters = fltr1 :? fltr2 :? NilFilters
        handler1 e1 = do
            liftIO $ putMVar var (Left e1)
            return TerminateEvent
        handler2 e2 = do
            liftIO $ putMVar var (Right e2)
            return TerminateEvent
        handlers = H handler1 :& H handler2 :& RNil
    _ <- web3 $ multiEvent filters handlers
    return var

data EventTag = ETE1 | ETE2 | ETE3 | ETE4
    deriving (Eq, Read, Show)

instance {-# OVERLAPPING #-} Ord (EventTag, Integer, Integer) where
    (_, b1, t1) `compare` (_, b2, t2) =
        let bCmp = b1 `compare` b2
        in if bCmp == EQ then t1 `compare` t2
                         else bCmp

monitorAllFourMulti :: Address
                    -> IO (TQueue (EventTag, Integer, Integer))
monitorAllFourMulti addr = do
    q <- newTQueueIO
    let f :: forall a. Default (Filter a) => Filter a
        f = defFilter addr
        h = enqueueingHandler q
        filters = f @E1 :? f @E2 :? f @E3 :? f @E4 :? NilFilters
        handlers = h ETE1 :& h ETE2 :& h ETE3 :& h ETE4 :& RNil
    void . web3 $ multiEvent filters handlers
    return q

monitorAllFourPar :: Address
                  -> IO (TQueue (EventTag, Integer, Integer))
monitorAllFourPar addr = do
    q <- newTQueueIO
    let f :: forall a. Default (Filter a) => Filter a
        f = defFilter addr
        h = enqueueingHandler q
        unH (H h) = h

    void . forkIO . web3 $ event' (f @E1) (unH $ h ETE1)
    void . forkIO . web3 $ event' (f @E2) (unH $ h ETE2)
    void . forkIO . web3 $ event' (f @E3) (unH $ h ETE3)
    void . forkIO . web3 $ event' (f @E4) (unH $ h ETE4)
    return q

defFilter :: forall a. Default (Filter a) => Address -> Filter a
defFilter addr = (def :: Filter a) { filterAddress = Just [addr] }

enqueueingHandler :: forall a. TQueue (EventTag, Integer, Integer)
                  -> EventTag
                  -> Handler (ReaderT Change Web3 EventAction) a
enqueueingHandler q tag = H . const $ do
    Change{..} <- ask
    let bn = unQuantity $ fromJust changeBlockNumber
        li = unQuantity $ fromJust changeLogIndex
    liftIO . atomically $ writeTQueue q (tag, bn, li)
    return ContinueEvent
