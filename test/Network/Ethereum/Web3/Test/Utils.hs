{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Ethereum.Web3.Test.Utils where

import           Control.Concurrent            (threadDelay)
import           Control.Exception             (SomeException, catch)
import           Control.Monad.IO.Class        (liftIO)
import           Data.Maybe                    (fromMaybe)
import           Data.Ratio                    (numerator)
import           Data.Time.Clock.POSIX         (getPOSIXTime)
import           Lens.Micro                    ((.~))
import           Network.HTTP.Client           (Manager, defaultManagerSettings,
                                                managerConnCount,
                                                managerRawConnection,
                                                managerRetryableException,
                                                newManager)
import           System.Environment            (lookupEnv)
import           System.IO.Unsafe              (unsafePerformIO)

import           Data.Solidity.Prim.Address    (Address)
import           Network.Ethereum.Account      (DefaultAccount, to, withAccount,
                                                withParam)
import           Network.Ethereum.Api.Eth      (accounts, blockNumber)
import           Network.Ethereum.Api.Provider (Provider (..), Web3,
                                                runWeb3With)
import           Network.Ethereum.Api.Types    (Quantity)

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

rpcUri :: IO String
rpcUri = liftIO (fromMaybe "http://localhost:8545" <$> lookupEnv "WEB3_PROVIDER")

contract :: Address
         -> DefaultAccount Web3 a
         -> IO a
contract a = web3 . withAccount () . withParam (to .~ a)

web3 :: Web3 a -> IO a
web3 ma = do
    provider <- HttpProvider <$> rpcUri
    v <- runWeb3With sharedManager provider ma
    case v of
        Left e   -> print e >> threadDelay 1000000 >> web3 ma
        Right v' -> return v'

withAccounts :: ([Address] -> IO a) -> IO a
withAccounts f = web3 accounts >>= f

withPrimaryEthereumAccount :: IO Address
withPrimaryEthereumAccount = withAccounts (pure . head)

sleepSeconds :: Int -> IO ()
sleepSeconds = threadDelay . (* 1000000)

microtime :: IO Integer
microtime = numerator . toRational . (* 1000000) <$> getPOSIXTime

awaitBlock :: Quantity -> IO ()
awaitBlock bn = do
    bn' <- web3 blockNumber
    -- putStrLn $ "awaiting block " ++ show bn ++ ", currently " ++ show bn'
    if bn' >= bn
        then return ()
        else threadDelay 1000000 >> awaitBlock bn

sleepBlocks :: Int -> IO ()
sleepBlocks n = do
    now <- web3 blockNumber
    awaitBlock $ now + (fromIntegral n)
