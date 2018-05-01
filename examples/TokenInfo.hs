{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Default                 (def)
import           Data.Text                    (unpack)
import           Text.Printf                  (printf)

import           Network.Ethereum.Contract.TH
import           Network.Ethereum.Web3        hiding (name)

import           ERC20

main :: IO ()
main = do
    result <- runWeb3 $ do
        n <- name tokenCall
        s <- symbol tokenCall
        d <- decimals tokenCall
        return $ printf "Token %s with symbol %s and decimals %d"
                   (unpack n) (unpack s) (fromIntegral d :: Int)
    case result of
      Left err   -> print err
      Right info -> putStrLn info
  where
    token :: Address
    token = "0xA2f4FCb0FDe2dD59f7a1873e121bc5623e3164Eb"

    tokenCall :: Call
    tokenCall = def { callTo = Just token }
