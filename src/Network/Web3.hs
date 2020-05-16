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

    -- * Ethereum Network
    , module Network.Ethereum

    -- * Polkadot Network
    , module Network.Polkadot

    -- * IPFS Network
    , module Network.Ipfs
    ) where

import qualified Network.Ethereum
import qualified Network.Ipfs
import qualified Network.Polkadot
import           Network.Web3.Provider (Web3, runWeb3)
