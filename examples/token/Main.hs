{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Default             (def)
import           Data.Text                (unpack)
import           Text.Printf              (printf)

import           Lens.Micro               ((.~))
import           Network.Ethereum         hiding (name)
import           Network.Ethereum.Account
import           Network.Web3

import           ERC20

main :: IO ()
main = do
    result <- runWeb3 $
        withAccount () $
            withParam (to .~ "0xA2f4FCb0FDe2dD59f7a1873e121bc5623e3164Eb") $ do
                n <- name
                s <- symbol
                d <- decimals
                return $ printf "Token %s with symbol %s and decimals %d"
                   (unpack n) (unpack s) (fromIntegral d :: Int)
    case result of
      Left err   -> error (show err)
      Right info -> putStrLn info
