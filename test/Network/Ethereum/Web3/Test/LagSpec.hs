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
import           Data.Either                      (isRight)
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
tagsSpec = describe "can interact with a SimpleStorage contract" $ do
    -- todo: this should ideally be arbitrary!
    it "can index from past to future and always stay 3 block behind or more" $ \(ContractsEnv{simpleStorage}, primaryAccount) -> do
      startingBlock <- runWeb3Configured $ do
        bn <- Eth.blockNumber
        setOneValuePerBlock primaryAccount simpleStorage [1..10]
        pure bn
      v <- newIORef (Right 0)
      let filter = (def :: Filter EvT_CountSet) {filterFromBlock = BlockWithNumber startingBlock}
      f <- runWeb3Configured' $ forkWeb3 $ eventNoFilter' filter 2 (lagHandler 3 20 v)
      runWeb3Configured $ setOneValuePerBlock primaryAccount simpleStorage [11..20]
      _ <- wait f
      result <- readIORef v
      result `shouldBe` Right 20

lagHandler
  :: Integer -- lag
  -> Integer -- max value
  -> IORef (Either (Quantity, Quantity) Integer)
  -> (EvT_CountSet -> ReaderT Change Web3 EventAction)
lagHandler lag maxInt resultVar = \(EvT_CountSet n) -> do
  chainHead <- lift $ Eth.blockNumber
  Change{..} <- ask
  let bn = fromJust changeBlockNumber
  if chainHead - bn < fromInteger lag
    then do
      liftIO $ writeIORef resultVar (Left (chainHead, bn))
      pure TerminateEvent
    else do
      let n' = toInteger n
      liftIO $ writeIORef resultVar (Right n')
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
