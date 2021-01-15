{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.IO.Class      (liftIO)

import           Network.Polkadot
import qualified Network.Polkadot.Rpc.Chain  as Chain
import qualified Network.Polkadot.Rpc.State  as State
import qualified Network.Polkadot.Rpc.System as System
import           Network.Web3.Provider       (Provider (..), runWeb3')

main :: IO ()
main = do
    result <- runWeb3' (HttpProvider "http://127.0.0.1:9933") $ do
        name <- System.name
        liftIO . putStrLn $ "System name: " ++ show name

        best <- Chain.getBlockHash Nothing
        liftIO . putStrLn $ "Best hash: " ++ show best

        Right now <- query "timestamp" "now" []
        liftIO . putStrLn $ "Timestamp: " ++ show (now :: Moment)

        Right total <- query "balances" "totalIssuance" []
        liftIO . putStrLn $ "Total amount: " ++ show (total :: Balance)

        let alice = "0xd43593c715fdd31c61141abd04a99fd6822c8558854ccde39a5684e7a56da27d" :: AccountId
        Right account <- query "system" "account" [Argument alice]
        liftIO . putStrLn $ "Alice account: " ++ show (account :: AccountInfo)

        State.getRuntimeVersion best

    case result of
      Left err      -> error (show err)
      Right version -> putStrLn (show version)
