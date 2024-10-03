-- |
-- Module      :  Network.Web3
-- Copyright   :  Aleksandr Krupenkin 2016-2024
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- Client library for Third Generation of Web.
--

module Network.Web3
    (
    -- * Web3 library uses JSON-RPC over WebSocket/HTTP(S) to access node functionality.
      Web3
    , runWeb3
    -- * Re-export popular Web3 platforms.
    , module Network.Ethereum
    , module Network.Polkadot
    ) where

import           Network.Ethereum
import           Network.Polkadot
import           Network.Web3.Provider (Web3, runWeb3)
