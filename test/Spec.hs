{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DataKinds         #-}
module Main where

import Network.Ethereum.Web3.Address (fromText)
import Network.Ethereum.Web3.TH
import Network.Ethereum.Web3

import Control.Concurrent (threadDelay)
import Data.ByteArray (Bytes)
import Data.Text (Text)

[abiFrom|data/sample.json|]

main :: IO ()
main = do
    putStrLn ""
    putStrLn [abiFrom|data/sample.json|]
    runWeb3 $ event addr (\(Action2 x y) -> print x >> print y >> return TerminateEvent)
    threadDelay 100000000
    return ()
  where Right addr = fromText "0x19EE7966474b31225F71Ef8e36A71378a58a20E1"
