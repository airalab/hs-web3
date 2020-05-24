-- |
-- Module      :  Network.Web3
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- Haskell library for next generation of Web.
--

module Network.Web3
    (
    -- * Web3 library uses JSON-RPC over HTTP(S) to access node functionality.
      Web3
    , runWeb3
    -- ^ Single entry point for any Web3 family network.
    ) where

import           Network.Web3.Provider (Web3, runWeb3)
