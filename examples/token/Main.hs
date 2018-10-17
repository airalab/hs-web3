{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Default             (def)
import           Data.Text                (unpack)
import           Text.Printf              (printf)

import           Network.Ethereum.Account
import           Network.Ethereum.Web3    hiding (name)

import           ERC20

main :: IO ()
main = do
    result <- runWeb3 $
        withAccount () $
            airaToken $ do
                n <- name
                s <- symbol
                d <- decimals
                return $ printf "Token %s with symbol %s and decimals %d"
                   (unpack n) (unpack s) (fromIntegral d :: Int)
    case result of
      Left err   -> error err
      Right info -> putStrLn info
  where
    airaToken = withParam $ to .~ "0xA2f4FCb0FDe2dD59f7a1873e121bc5623e3164Eb"
