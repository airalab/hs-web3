{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE RecordWildCards      #-}

module Network.Ethereum.Web3.Test.LagSpec where

import           Control.Concurrent               (threadDelay)
import           Control.Concurrent.Async         (wait)
import           Control.Concurrent.MVar
import           Control.Monad                    (void)
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Reader       (ReaderT, ask)
import           Data.ByteString                  (ByteString)
import           Data.Default
import           Data.Either                      (isRight, isLeft)
import           Data.Foldable                    (forM_)
import           Data.List                        (sort)
import           Data.Maybe
import           Data.Monoid
import           Data.String                      (fromString)
import qualified Data.Text                        as T
import           Data.Traversable                 (for)
import           GHC.TypeLits

import           Network.Ethereum.Contract.TH
import           Network.Ethereum.Contract.Event
import           Network.Ethereum.Web3            hiding (convert)
import qualified Network.Ethereum.Web3.Eth        as Eth
import           Network.Ethereum.Web3.Provider   (forkWeb3)
import           Network.Ethereum.Web3.Types

import           Numeric                          (showHex)
import           System.Environment               (getEnv)
import           System.IO.Unsafe                 (unsafePerformIO)
import           Test.Hspec
import           Data.IORef
import           Network.Ethereum.Web3.Test.Utils

[abiFrom|test-support/build/contracts/abis/SimpleStorage.json|]

unEvT_CountSet :: EvT_CountSet -> UIntN 256
unEvT_CountSet (EvT_CountSet n) = n

spec :: Spec
spec = makeEnv `before` tagsSpec

tagsSpec :: SpecWith (ContractsEnv, Address)
tagsSpec = describe "can lag behind chain head for SimpleStorage contract" $ do
    -- This test fires of 10 SetCount events, then forks a filter to index these past events while firing off new ones.
    -- We set the lag to always be at least 3 blocks behind the chain head, and we set the historical window size to 5.
    -- We then check that all events were indexed and that the lag worked. We also check that the lag number is tight,
    -- meaning that we do get to (chainHead - lag).
    it "can index from past to future and always lag 3 block behind or more" $ \(ContractsEnv{simpleStorage}, primaryAccount) -> do
      startingBlock <- runWeb3Configured $ do
        bn <- Eth.blockNumber
        setOneValuePerBlock primaryAccount simpleStorage [1..10]
        pure bn
      v <- newIORef (Right (0, fromInteger 0))
      let filter = (def :: Filter EvT_CountSet) {filterFromBlock = BlockWithNumber startingBlock}
          lag = 2
      f <- runWeb3Configured' $ forkWeb3 $ eventManyNoFilter' filter 5 lag (lagHandler lag 20 v)
      runWeb3Configured $ setOneValuePerBlock primaryAccount simpleStorage [11..20]
      _ <- wait f
      result <- readIORef v
      result `shouldBe` Right (20, fromInteger lag)
    -- this is the control test, we want to know that if we set the lag to 0 that we eventually catch up to chain head.
    -- it's pretty much copy pasta
    it "the same test without specifying a positive lag will catch up to chain head" $ \(ContractsEnv{simpleStorage}, primaryAccount) -> do
      startingBlock <- runWeb3Configured $ do
        bn <- Eth.blockNumber
        setOneValuePerBlock primaryAccount simpleStorage [1..10]
        pure bn
      v <- newIORef (Right (0, fromInteger 0))
      let filter = (def :: Filter EvT_CountSet) {filterFromBlock = BlockWithNumber startingBlock}
          lag = 2
      f <- runWeb3Configured' $ forkWeb3 $ eventManyNoFilter' filter 3 0 (lagHandler lag 20 v)
      runWeb3Configured $ setOneValuePerBlock primaryAccount simpleStorage [11..20]
      _ <- wait f
      result <- readIORef v
      result `shouldSatisfy` isLeft

lagHandler
  :: Integer -- lag
  -> Integer -- max value
  -> IORef (Either (Quantity, Quantity) (Integer, Quantity))
  -> (EvT_CountSet -> ReaderT Change Web3 EventAction)
lagHandler lag maxInt resultVar = \(EvT_CountSet n) -> do
  chainHead <- lift $ Eth.blockNumber
  Change{..} <- ask
  let bn = fromJust changeBlockNumber
      blockDiff = chainHead - bn
  if  blockDiff < fromInteger lag
    then do
      liftIO $ writeIORef resultVar (Left (chainHead, bn))
      pure TerminateEvent
    else do
      let n' = toInteger n
      liftIO $ writeIORef resultVar (Right (n', blockDiff))
      if n' == maxInt
        then pure TerminateEvent
        else pure ContinueEvent

setOneValuePerBlock
  :: Address
  -> Address
  -> [Int]
  -> Web3 ()
setOneValuePerBlock sender ssAddress ns =
    let txOpts = def { callTo = Just ssAddress
                     , callFrom = Just sender
                     }
    in forM_ ns $ \i -> do
         _ <- setCount txOpts (fromIntegral i)
         n <- Eth.blockNumber
         pollTillNewBlock n
  where
    pollTillNewBlock n = do
      n' <- Eth.blockNumber
      if (n' > n)
        then do
          liftIO $ threadDelay 1000000
          pollTillNewBlock n
        else pure ()
