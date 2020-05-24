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

-- Module      :  Network.Ethereum.Test.SimpleStorageSpec
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
--

module Network.Ethereum.Test.SimpleStorageSpec where

import           Control.Concurrent.Async     (wait)
import           Control.Concurrent.MVar
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Class    (lift)
import           Control.Monad.Trans.Reader   (ask)
import           Data.Default                 (def)
import           Data.List                    (sort)
import           Data.Monoid                  ((<>))
import           Lens.Micro                   ((.~))
import           Test.Hspec

import           Network.Ethereum
import qualified Network.Ethereum.Api.Eth     as Eth
import           Network.Ethereum.Api.Types
import           Network.Ethereum.Contract    (new)
import           Network.Ethereum.Contract.TH
import           Network.Web3
import           Network.Web3.Provider        (forkWeb3)

import           Network.Ethereum.Test.Utils

[abiFrom|test/contracts/SimpleStorage.json|]

unEvT_CountSet :: EvT_CountSet -> UIntN 256
unEvT_CountSet (EvT_CountSet n) = n

deploy :: IO Address
deploy = do
    Just address <- web3 $ withAccount () $ withParam id $ new SimpleStorageContract
    putStrLn $ "SimpleStorage: " ++ show address
    return address

spec :: Spec
spec = deploy `before` do
  interactions
  events

interactions :: SpecWith Address
interactions = describe "can interact with a SimpleStorage contract" $ do
    -- todo: this should ideally be arbitrary!
    let theValue = 12345

    it "can set the value of a SimpleStorage contract and read the value back" $ \storage -> do
        _ <- contract storage $ setCount theValue

        now <- web3 Eth.blockNumber
        let later = now + 3
        awaitBlock later

        v <- contract storage count
        v `shouldBe` theValue

    it "can set transaction gas limit" $ \storage -> do
        TxReceipt{..} <- contract storage $ withParam (gasLimit .~ 500000) $ setCount theValue
        Just Transaction{..} <- web3 $ Eth.getTransactionByHash receiptTransactionHash
        txGas `shouldBe` 500000

    it "can estimate transaction gas limit" $ \storage -> do
        TxReceipt{..} <- contract storage $ setCount theValue
        Just Transaction{..} <- web3 $ Eth.getTransactionByHash receiptTransactionHash
        txGas `shouldBe` 42822

events :: SpecWith Address
events = do
    describe "can interact with a SimpleStorage contract across block intervals" $ do
        it "can stream events starting and ending in the future, unbounded" $ \storage -> do
            var <- newMVar []
            let theSets = [1, 2, 3]
            putStrLn "Setting up the filter..."
            fiber <- web3 $ do
                let fltr = (def :: Filter EvT_CountSet) { filterAddress = Just [storage] }
                forkWeb3 $ processUntil' var fltr ((3 ==) . length)
            putStrLn "Setting the values..."
            setValues storage theSets
            wait fiber
            putStrLn "Filter caught 3 values..."
            vals <- takeMVar var
            sort (unEvT_CountSet <$> vals) `shouldBe` sort theSets

        it "can stream events starting and ending in the future, bounded" $ \storage -> do
            var <- newMVar []
            let theSets = [13, 14, 15]
            start <- web3 Eth.blockNumber
            let later = BlockWithNumber (start + 3)
                latest = BlockWithNumber (start + 8)
                fltr = (def :: Filter EvT_CountSet) { filterAddress = Just [storage]
                                                    , filterFromBlock = later
                                                    , filterToBlock = latest
                                                    }
            putStrLn "Setting up the filter..."
            fiber <- web3 $
                forkWeb3 $ processUntil' var fltr ((3 ==) . length)
            awaitBlock (start + 3)
            putStrLn "Setting the values..."
            setValues storage theSets
            wait fiber
            putStrLn "Filter caught 3 values..."
            vals <- takeMVar var
            sort (unEvT_CountSet <$> vals) `shouldBe` init (sort theSets)

        it "can stream events starting in the past and ending in the future" $ \storage -> do
            var <- newMVar []
            blockNumberVar <- newEmptyMVar
            let theSets1 = [7, 8, 9]
                theSets2 = [10, 11, 12]
            start <- web3 Eth.blockNumber
            let fltr = (def :: Filter EvT_CountSet) { filterAddress = Just [storage] }
            fiber <- web3 $ do
                forkWeb3 $ processUntil var fltr ((3 ==) . length) (liftIO . putMVar blockNumberVar . changeBlockNumber)
            putStrLn "Running first transactions as past transactions..."
            setValues storage theSets1
            wait fiber
            putStrLn "All past transactions succeeded... "
            Just end <- takeMVar blockNumberVar
            awaitBlock $ end + 1  -- make past transactions definitively in past
            var' <- newMVar []
            fiber' <- web3 $ do
                let fltr' = (def :: Filter EvT_CountSet) { filterAddress = Just [storage]
                                                        , filterFromBlock = BlockWithNumber start}
                forkWeb3 $ processUntil' var' fltr' ((6 ==) . length)
            putStrLn "Setting more values"
            setValues storage theSets2
            wait fiber'
            putStrLn "All new values have ben set"
            vals <- takeMVar var'
            sort (unEvT_CountSet <$> vals) `shouldBe` sort (theSets1 <> theSets2)

    it "can stream events starting and ending in the past, bounded" $ \storage -> do
        var <- newMVar []
        let theSets = [4, 5, 6]
        start <- web3 Eth.blockNumber
        blockNumberVar <- newEmptyMVar
        let fltr = (def :: Filter EvT_CountSet) { filterAddress = Just [storage] }
        putStrLn "Setting up filter for past transactions..."
        fiber <- web3 $ do
          forkWeb3 $ processUntil var fltr ((3 ==) . length) (liftIO . putMVar blockNumberVar . changeBlockNumber)
        putStrLn "Setting values"
        setValues storage theSets
        wait fiber
        putStrLn "All values have been set"
        Just end <- takeMVar blockNumberVar
        var' <- newMVar []
        let fltr' = fltr { filterFromBlock = BlockWithNumber start
                         , filterToBlock = BlockWithNumber end
                         }
        awaitBlock $ end + 1  -- make it definitively in the past
        web3 $ processUntil' var' fltr' ((3 ==) . length)
        vals <- takeMVar var'
        sort (unEvT_CountSet <$> vals) `shouldBe` sort theSets

processUntil :: MVar [EvT_CountSet]
             -> Filter EvT_CountSet
             -> ([EvT_CountSet] -> Bool)  -- TODO: make it work for any event
             -> (Change -> Web3 ())
             -> Web3 ()
processUntil var fltr predicate action = do
  event fltr $ \(ev :: EvT_CountSet) -> do
    newV <- liftIO $ modifyMVar var $ \v -> return (ev:v, ev:v)
    if predicate newV
        then do
          change <- ask
          lift $ action change
          return TerminateEvent
        else return ContinueEvent

processUntil' :: MVar [EvT_CountSet]
              -> Filter EvT_CountSet
              -> ([EvT_CountSet] -> Bool)
              -> Web3 ()
processUntil' var fltr predicate = processUntil var fltr predicate (const $ return ())

setValues :: Address -> [UIntN 256] -> IO ()
setValues storage = mapM_ (contract storage . setCount)
