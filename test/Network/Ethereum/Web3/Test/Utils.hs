module Network.Ethereum.Web3.Test.Utils
  ( injectExportedEnvironmentVariables
  , runWeb3Configured
  , runWeb3Configured'
  , withAccounts
  , withPrimaryEthereumAccount
  , callFromTo
  , sleepSeconds
  , microtime
  , awaitBlock
  ) where

import           Control.Concurrent          (MVar, threadDelay, tryTakeMVar)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Default
import           Data.Either                 (isRight)
import           Data.List.Split             (splitOn)
import           Data.Maybe                  (fromMaybe)
import           Data.Ratio                  (numerator)
import           Data.String                 (IsString, fromString)
import qualified Data.Text                   as T
import           Data.Time.Clock.POSIX       (getPOSIXTime)
import           Data.Traversable            (for)
import           Network.Ethereum.Web3       (Address, DefaultProvider,
                                              Provider (..), Web3, Web3Error,
                                              runWeb3')
import           Network.Ethereum.Web3.Eth   (accounts, blockNumber)
import           Network.Ethereum.Web3.Types (BlockNumber, Call (..))
import           System.Environment          (lookupEnv, setEnv)
import           Test.Hspec.Expectations     (shouldSatisfy)

data EnvironmentProvider

instance Provider EnvironmentProvider where
    rpcUri = liftIO (fromMaybe "http://localhost:8545" <$> lookupEnv "WEB3_PROVIDER")

exportStore :: String
exportStore = "./test-support/.detected-contract-addresses"

loadExportedEnvironmentVariables :: IO [(String, String)]
loadExportedEnvironmentVariables = do
    exportables <- lines <$> readFile exportStore
    detecteds <- for exportables $ \line -> case words line of
        ["export", e] -> case splitOn "=" e of
            [x]    -> detectedEnv x ""
            [k, v] -> detectedEnv k v
            _      -> warnMalformation $ "oddly structured export statement " ++ line
        _ -> warnMalformation $ "no export in line " ++ line
    return $ concat detecteds

    where warnMalformation m = do
            putStrLn $ m ++ ". are you sure you're using the right inject-contract-addresses.sh?"
            pure []
          detectedEnv k v = do
              -- putStrLn $ "detected " ++ k ++ "=" ++ v
              pure [(k, v)]

injectExportedEnvironmentVariables :: IO ()
injectExportedEnvironmentVariables = do
    detectedEnvs <- loadExportedEnvironmentVariables
    sequence_ (uncurry setEnv <$> detectedEnvs)

runWeb3Configured :: Show a => Web3 EnvironmentProvider a -> IO a
runWeb3Configured f = do
    v <- runWeb3' f
    v `shouldSatisfy` isRight
    let Right a = v in return a

runWeb3Configured' :: Web3 EnvironmentProvider a -> IO a
runWeb3Configured' f = do
    Right v <- runWeb3' f
    return v

withAccounts :: ([Address] -> IO a) -> IO a
withAccounts f = runWeb3Configured accounts >>= f

withPrimaryEthereumAccount :: IO Address
withPrimaryEthereumAccount = withAccounts (pure . head)

callFromTo :: Address -> Address -> Call
callFromTo from to =
    def { callFrom = Just from
        , callTo   = to
        , callGasPrice = Just 4000000000
        }

sleepSeconds :: Int -> IO ()
sleepSeconds = threadDelay . (* 1000000)

microtime :: IO Integer
microtime = numerator . toRational . (* 1000000) <$> getPOSIXTime

awaitBlock :: BlockNumber -> IO ()
awaitBlock bn = do
    bn' <- runWeb3Configured blockNumber
    putStrLn $ "awaiting block " ++ show bn ++ ", currently " ++ show bn'
    if bn' >= bn
        then return ()
        else threadDelay 1000000 >> awaitBlock bn
