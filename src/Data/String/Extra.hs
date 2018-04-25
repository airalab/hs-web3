-- |
-- Module      :  Data.String.Extra
-- Copyright   :  Alexander Krupenkin 2016-2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Extra string manipulation functions.
--

module Data.String.Extra where

import           Data.Char (toLower, toUpper)

-- | Lower first char of string
toLowerFirst :: String -> String
toLowerFirst []       = []
toLowerFirst (x : xs) = toLower x : xs

-- | Upper first char of string
toUpperFirst :: String -> String
toUpperFirst []       = []
toUpperFirst (x : xs) = toUpper x : xs
