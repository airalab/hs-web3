{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Main where

import           Data.Text                (unpack)
import           Network.Ethereum.Web3
import           Network.Ethereum.Web3.TH
import           Text.Printf

[abiFrom|data/ERC20.json|]

main :: IO ()
main = do
    putStrLn ""
    putStrLn [abiFrom|data/ERC20.json|]
{-
    Right s <- runWeb3 $ do
        n <- name token
        s <- symbol token
        d <- decimals token
        return $ printf "Token %s with symbol %s and decimals %d"
                        (unpack n) (unpack s) d
    putStrLn s
  where token = "0x237D60A8b41aFD2a335305ed458B609D7667D789"
-}
