{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network.Ethereum.Web3.Test.SimpleStorageSpec where

import           Control.Concurrent               (threadDelay)
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
import           Network.Ethereum.Web3.Types      (Call (..), Change (..), Filter (..))
import           Numeric                          (showHex)
import           System.Environment               (getEnv)
import           System.IO.Unsafe                 (unsafePerformIO)
import           Test.Hspec

[abiFrom|build/contracts/abis/SimpleStorage.json|]

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
        now' <- runWeb3Configured Eth.blockNumber
        let now = read (T.unpack now')
            later = now + 3
        awaitBlock later
        v <- runWeb3Configured (count theCall)
        v `shouldBe` theValue

events :: SpecWith Address
events = describe "can interact with a SimpleStorage contract across block intervals" $ do
    it "can stream events starting and ending in the future, unbounded" $ \primaryAccount -> do
        var <- newMVar []
        termination <- newEmptyMVar
        let theCall = callFromTo primaryAccount contractAddress
            theSets = [8, 9, 10]
        now' <- runWeb3Configured Eth.blockNumber
        let now = read (T.unpack now')
            later = now + 3
        liftIO . putStrLn $ "now is " ++ show now ++ " (" ++ show now' ++ ")"
        void . runWeb3Configured $ event contractAddress $ \(CountSet cs) -> do
            liftIO . print $ "Got count: " ++ show cs
            v <- liftIO $ takeMVar var
            let newV = cs : v
            liftIO $ putMVar var newV
            if length newV == 3
                then do
                    liftIO $ putMVar termination True
                    return TerminateEvent
                else do
                  return ContinueEvent
        awaitBlock later
        void . for theSets $ \v -> runWeb3Configured (setCount theCall v)
        takeMVarWithTimeout 20000000 termination >>= \case
            Nothing -> error "timed out waiting for event thread!"
            Just term -> return ()
        vals <- takeMVar var
        sort vals `shouldBe` sort theSets
