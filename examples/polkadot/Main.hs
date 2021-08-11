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

        me <- liftIO generate
        liftIO . putStrLn $ "My account: " ++ show (multi_signer (me :: Ed25519))

        liftIO $ putStrLn "Please send 1000 Unit and press any key"
        liftIO getLine

        Right account <- query "system" "account" [Argument (into_account $ multi_signer me)]
        liftIO . putStrLn $ "My account: " ++ show (account :: AccountInfo)

        transfer <- new_call "Balances" "transfer" (MaId alice, Compact 200000000000000)
        liftIO . putStrLn $ "Sign and send 200 Unit transfer transaction: " ++ show transfer
        sign_and_send me transfer 0

    case result of
      Left err      -> error (show err)
      Right tx_hash -> putStrLn ("Extrinsic sent " ++ show tx_hash)
