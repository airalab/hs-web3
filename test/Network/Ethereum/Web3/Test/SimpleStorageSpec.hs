{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
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
import           Network.Ethereum.Web3.Encoding   (ABIEncoding (..))
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

newtype BoundedEvent ev (from :: Symbol) (to :: Symbol) = BoundedEvent ev

instance ABIEncoding ev => ABIEncoding (BoundedEvent ev from to) where
    fromDataParser = BoundedEvent <$> fromDataParser
    toDataBuilder (BoundedEvent ev) = toDataBuilder ev

instance (Event ev, KnownSymbol from, KnownSymbol to) => Event (BoundedEvent ev from to) where
    eventFilter (BoundedEvent ev) a =
        let maybifyBlank "" = Nothing
            maybifyBlank x  = Just (T.pack x)
            baseFilter = eventFilter ev a
         in baseFilter { filterFromBlock = maybifyBlank $ symbolVal (Proxy :: Proxy from)
                       , filterToBlock = maybifyBlank $ symbolVal (Proxy :: Proxy to)
                       }

instance Show ev => Show (BoundedEvent ev from to) where
    show (BoundedEvent ev) = show ev

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
        sleepSeconds 5
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
            later' = "0x" ++ showHex later ""
        liftIO . putStrLn $ "now is " ++ show now ++ " (" ++ show now' ++ ")"
        SomeSymbol (Proxy :: Proxy later) <- return (someSymbolVal later')
        void . runWeb3Configured $ event contractAddress $ \(ev :: BoundedEvent CountSet later "latest") -> do
            let BoundedEvent (CountSet cs) = ev
            liftIO . putStrLn $ "1: Got a CountSet! " ++ show cs
            liftIO $ modifyMVar_ var (return . (cs:))
            if cs == 10
                then do
                    liftIO $ putMVar termination True
                    return TerminateEvent
                else return ContinueEvent
        awaitBlock later
        void . for theSets $ \v -> runWeb3Configured (setCount theCall v)
        takeMVarWithTimeout 20000000 termination >>= \case
            Nothing -> error "timed out waiting for filter!"
            Just term -> return ()
        vals <- takeMVar var
        sort vals `shouldBe` sort theSets
    it "can stream events starting and ending in the future, bounded" $ \primaryAccount -> do
        var <- newMVar []
        termination <- newEmptyMVar
        let theCall = callFromTo primaryAccount contractAddress
            theSets = [10, 11, 12]
        now' <- runWeb3Configured Eth.blockNumber
        let now = read (T.unpack now')
            later = now + 3
            latest = now + 8
            later' = "0x" ++ showHex later ""
            latest' = "0x" ++ showHex latest ""
        liftIO . putStrLn $ "now is " ++ show now ++ " (" ++ show now' ++ ")"
        SomeSymbol (Proxy :: Proxy later) <- return (someSymbolVal later')
        SomeSymbol (Proxy :: Proxy latest) <- return (someSymbolVal latest')
        void . runWeb3Configured $ event contractAddress $ \(ev :: BoundedEvent CountSet later latest) -> do
            let BoundedEvent (CountSet cs) = ev
            liftIO . putStrLn $ "1: Got a CountSet! " ++ show cs
            liftIO $ modifyMVar_ var (return . (cs:))
            if cs == 12
                then do
                    liftIO $ putMVar termination True
                    return TerminateEvent
                else return ContinueEvent
        awaitBlock later
        void . for theSets $ \v -> runWeb3Configured (setCount theCall v)
        takeMVarWithTimeout 20000000 termination >>= \case
            Nothing -> error "timed out waiting for filter!"
            Just term -> return ()
        vals <- takeMVar var
        sort vals `shouldBe` sort theSets
