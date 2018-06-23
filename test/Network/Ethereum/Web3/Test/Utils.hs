{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Network.Ethereum.Web3.Test.Utils
  ( makeEnv
  , ContractsEnv(..)
  , runWeb3Configured
  , runWeb3Configured'
  , withAccounts
  , withPrimaryEthereumAccount
  , callFromTo
  , sleepSeconds
  , microtime
  , awaitBlock
  ) where

import           Control.Concurrent                (MVar, threadDelay,
                                                    tryTakeMVar)
import           Control.Monad.IO.Class            (liftIO)
import           Data.Aeson                        (FromJSON, eitherDecode)
import           Data.Aeson.Types                  (Value(..))
import           Data.Aeson.Lens                   (_Object, key, _JSON)
import qualified Data.ByteString.Lazy              as BSL
import           Data.Default
import           Data.Either                       (isRight)
import           Control.Lens                      ((^?))
import           Data.List.Split                   (splitOn)
import           Data.Maybe                        (fromMaybe)
import           Data.Ratio                        (numerator)
import           Data.String                       (IsString, fromString)
import qualified Data.Text                         as T
import           Data.Time.Clock.POSIX             (getPOSIXTime)
import           Data.Traversable                  (for)
import           GHC.Generics                      (Generic)
import           Network.Ethereum.ABI.Prim.Address (Address)
import           Network.Ethereum.Web3.Eth         (accounts, blockNumber)
import           Network.Ethereum.Web3.Provider    (Provider (..), JsonRpcProvider(..)
                                                   , Web3, Web3Error, runWeb3')
import           Network.Ethereum.Web3.Types       (Call (..), Quantity)
import           System.Environment                (lookupEnv, setEnv)
import           Test.Hspec.Expectations           (shouldSatisfy)
import           Network.Ethereum.Web3.Net         as Net


makeEnv :: IO (ContractsEnv, Address)
makeEnv = do
  cenv <- makeContractsEnv
  a <- withPrimaryEthereumAccount
  pure (cenv, a)

rpcUri :: IO String
rpcUri =  liftIO (fromMaybe "http://localhost:8545" <$> lookupEnv "WEB3_PROVIDER")

data ContractsEnv =
  ContractsEnv { simpleStorage :: Address
               , complexStorage :: Address
               }

makeContractsEnv :: IO ContractsEnv
makeContractsEnv = do
    net <- runWeb3Configured' Net.version
    ss <- grabAddress net <$> BSL.readFile "test-support/build/contracts/abis/SimpleStorage.json"
    cs <- grabAddress net <$> BSL.readFile "test-support/build/contracts/abis/ComplexStorage.json"
    pure $ ContractsEnv ss cs
  where
    grabAddress :: T.Text -> BSL.ByteString -> Address
    grabAddress nid bs = case eitherDecode bs :: Either String Value of
      Right val -> fromMaybe (error "address key missing") (val ^? key "networks" . key nid . key "address" . _JSON)
      Left e -> error e

runWeb3Configured :: Show a => Web3 a -> IO a
runWeb3Configured f = do
    provider <- (flip Provider Nothing . HttpProvider) <$> rpcUri
    v <- runWeb3' provider f
    v `shouldSatisfy` isRight
    let Right a = v in return a

runWeb3Configured' :: Web3 a -> IO a
runWeb3Configured' f = do
    provider <- (flip Provider Nothing . HttpProvider) <$> rpcUri
    Right v <- runWeb3' provider f
    return v

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
    bn' <- runWeb3Configured blockNumber
    putStrLn $ "awaiting block " ++ show bn ++ ", currently " ++ show bn'
    if bn' >= bn
        then return ()
        else threadDelay 1000000 >> awaitBlock bn
