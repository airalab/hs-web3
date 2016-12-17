-- |
-- Module      :  Network.Ethereum.Web3.Internal
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Common used internal functions.
--
module Network.Ethereum.Web3.Internal where

import Data.Char (toLower, toUpper)

-- | Lower first char of string
toLowerFirst :: String -> String
toLowerFirst [] = []
toLowerFirst (x : xs) = toLower x : xs

-- | Upper first char of string
toUpperFirst :: String -> String
toUpperFirst [] = []
toUpperFirst (x : xs) = toUpper x : xs
