-- |
-- Module      :  Network.Web3
-- Copyright   :  Aleksandr Krupenkin 2016-2020
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
    ) where

import           Network.Web3.Provider (Web3, runWeb3)
