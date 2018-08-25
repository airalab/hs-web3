{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Ethereum.Web3.Test.Utils
  ( makeEnv
  , ContractsEnv(..)
  , runWeb3Configured
  , runWeb3Configured'
  , retryWeb3Configured
  , withAccounts
  , withPrimaryEthereumAccount
  , callFromTo
  , sleepSeconds
  , microtime
  , awaitBlock
  , awaitTxMined
  , sleepBlocks
  ) where

import           Control.Concurrent                (MVar, threadDelay,
                                                    tryTakeMVar)
import           Control.Monad.IO.Class            (liftIO)
import           Data.Default
import           Data.Either                       (isRight)
import           Data.List.Split                   (splitOn)
import           Data.Maybe                        (fromMaybe)
import           Data.Ratio                        (numerator)
import           Data.String                       (IsString, fromString)
import qualified Data.Text                         as T
import           Data.Time.Clock.POSIX             (getPOSIXTime)
import           Data.Traversable                  (for)

import           Control.Concurrent                (MVar, threadDelay,
                                                    tryTakeMVar)
import           Control.Exception                 (SomeException, catch,
                                                    handle, throwIO)
import           Control.Lens                      ((^?))
import           Control.Monad.IO.Class            (liftIO)
import           Data.Aeson                        (FromJSON, eitherDecode)
import           Data.Aeson.Lens                   (key, _JSON, _Object)
import           Data.Aeson.Types                  (Value (..))
import qualified Data.ByteString.Lazy              as BSL
import           Data.Default
import           Data.Either                       (isRight)
import           Data.List.Split                   (splitOn)
import           Data.Maybe                        (fromMaybe, isJust)
import           Data.Ratio                        (numerator)
import           Data.Solidity.Prim.Address        (Address)
import           Data.String                       (IsString, fromString)
import qualified Data.Text                         as T
import           Data.Time.Clock.POSIX             (getPOSIXTime)
import           Data.Traversable                  (for)
import           GHC.Generics                      (Generic)
import           Network.Ethereum.ABI.Prim.Address (Address)
import           Network.Ethereum.Api.Eth          (accounts, blockNumber)
import           Network.Ethereum.Api.Provider     (Provider (..), Web3,
                                                    Web3Error, runWeb3')
import           Network.Ethereum.Api.Types        (Call (..), Quantity)
import           Network.Ethereum.Web3.Eth         (accounts, blockNumber,
                                                    getTransactionReceipt)
import           Network.Ethereum.Web3.Net         as Net
import           Network.Ethereum.Web3.Provider    (JsonRpcProvider (..),
                                                    Provider (..), Web3,
                                                    Web3Error, runWeb3With)
import           Network.Ethereum.Web3.Types       (Call (..), Hash, Quantity,
                                                    TxReceipt (..))
import           Network.HTTP.Client               (Manager, ManagerSettings,
                                                    defaultManagerSettings,
                                                    managerConnCount,
                                                    managerRawConnection,
                                                    managerRetryableException,
                                                    newManager)
import           System.Environment                (lookupEnv, setEnv)
import           System.Environment                (lookupEnv, setEnv)
import           System.IO.Unsafe                  (unsafePerformIO)
import           Test.Hspec.Expectations           (shouldSatisfy)
import           Test.Hspec.Expectations           (shouldSatisfy)

-- shared manager used throughout the helpers here to prevent hammering geth from ruining everything
-- this also retrys on ALL exceptions, including ConnectionResetByPeer and stuff like that
sharedManager :: Manager
sharedManager = unsafePerformIO $ newManager defaultManagerSettings
    { managerConnCount = 5
    , managerRetryableException = const False
    , managerRawConnection = fixRawConnection retryOpenConnection
    }

    where retryOpenConnection = threadDelay 500000 >> managerRawConnection defaultManagerSettings
          fixRawConnection f = f `catch` (\(_ :: SomeException) -> fixRawConnection f)
{-# NOINLINE sharedManager #-}

makeEnv :: IO (ContractsEnv, Address)
makeEnv = do
  cenv <- makeContractsEnv
  a <- withPrimaryEthereumAccount
  pure (cenv, a)

rpcUri :: IO String
rpcUri =  liftIO (fromMaybe "http://localhost:8545" <$> lookupEnv "WEB3_PROVIDER")

data ContractsEnv =
  ContractsEnv { simpleStorage  :: Address
               , complexStorage :: Address
               , linearization  :: Address
               }

makeContractsEnv :: IO ContractsEnv
makeContractsEnv = do
    net <- runWeb3Configured' Net.version
    ss <- grabAddress net <$> BSL.readFile "test-support/build/contracts/abis/SimpleStorage.json"
    cs <- grabAddress net <$> BSL.readFile "test-support/build/contracts/abis/ComplexStorage.json"
    lin <- grabAddress net <$> BSL.readFile "test-support/build/contracts/abis/Linearization.json"
    pure $ ContractsEnv ss cs lin
  where
    grabAddress :: T.Text -> BSL.ByteString -> Address
    grabAddress nid bs = case eitherDecode bs :: Either String Value of
      Right val -> fromMaybe (error "address key missing") (val ^? key "networks" . key nid . key "address" . _JSON)
      Left e -> error e

runWeb3Configured :: Show a => Web3 a -> IO a
runWeb3Configured f = do
    provider <- HttpProvider <$> rpcUri
    v <- runWeb3With sharedManager provider f
    v `shouldSatisfy` isRight
    let Right a = v in return a

runWeb3Configured' :: Web3 a -> IO a
runWeb3Configured' f = do
    provider <- HttpProvider <$> rpcUri
    Right v <- runWeb3With sharedManager provider f
    return v

retryWeb3Configured :: Web3 a -> IO a
retryWeb3Configured f = do
    provider <- (flip Provider Nothing . HttpProvider) <$> rpcUri
    v <- runWeb3With sharedManager provider f
    case v of
        Left _  -> threadDelay 1000000 >> retryWeb3Configured f
        Right v -> return v

withAccounts :: ([Address] -> IO a) -> IO a
withAccounts f = runWeb3Configured accounts >>= f

withPrimaryEthereumAccount :: IO Address
withPrimaryEthereumAccount = withAccounts (pure . head)

callFromTo :: Address -> Address -> Call
callFromTo from to =
    def { callFrom = Just from
        , callTo   = Just to
        , callGasPrice = Just 4000000000
        }

sleepSeconds :: Int -> IO ()
sleepSeconds = threadDelay . (* 1000000)

microtime :: IO Integer
microtime = numerator . toRational . (* 1000000) <$> getPOSIXTime

awaitBlock :: Quantity -> IO ()
awaitBlock bn = do
    bn' <- retryWeb3Configured blockNumber
    -- putStrLn $ "awaiting block " ++ show bn ++ ", currently " ++ show bn'
    if bn' >= bn
        then return ()
        else threadDelay 1000000 >> awaitBlock bn

sleepBlocks :: Int -> IO ()
sleepBlocks n = do
    now <- retryWeb3Configured blockNumber
    awaitBlock $ now + (fromIntegral n)

awaitTxMined :: Hash -> IO TxReceipt
awaitTxMined hash = do
    mReceipt <- retryWeb3Configured $ getTransactionReceipt hash
    case mReceipt of
        Nothing -> threadDelay 1000000 >> awaitTxMined hash
        Just receipt -> if isJust (receiptBlockHash receipt)
          then return receipt
          else threadDelay 1000000 >> awaitTxMined hash
