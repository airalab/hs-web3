{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.IO.Class      (liftIO)

import qualified Network.Polkadot.Rpc.Chain  as Chain
import qualified Network.Polkadot.Rpc.State  as State
import qualified Network.Polkadot.Rpc.System as System
import           Network.Web3.Provider       (Provider (..), runWeb3')

main :: IO ()
main = do
    result <- runWeb3' (WsProvider "127.0.0.1" 9944) $ do
        name <- System.name
        liftIO . putStrLn $ "System name: " ++ show name

        best <- Chain.getBlockHash Nothing
        liftIO . putStrLn $ "Best hash: " ++ show best

        State.getRuntimeVersion best

    case result of
      Left err      -> error (show err)
      Right version -> putStrLn (show version)
