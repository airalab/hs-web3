-- |
-- Module      :  Network.Ethereum.Web3
-- Copyright   :  Alexander Krupenkin 2016-2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- An Ethereum node offers a RPC interface. This interface gives Ðapp’s
-- access to the Ethereum blockchain and functionality that the node provides,
-- such as compiling smart contract code. It uses a subset of the JSON-RPC 2.0
-- specification (no support for notifications or named parameters) as serialisation
-- protocol and is available over HTTP and IPC (unix domain sockets on linux/OSX
-- and named pipe’s on Windows).
--
-- Web3 Haskell library currently use JSON-RPC over HTTP to access node functionality.
--

module Network.Ethereum.Web3
    (
    -- * Base monad for any Ethereum node communication
      Web3
    , runWeb3

    -- * Basic transaction sending
    , module Account

    -- * Basic event listening
    , EventAction(..)
    , event

    -- * Primitive data types
    , module Prim

    -- * Metric unit system
    , module Unit
    ) where

import           Data.Solidity.Prim              as Prim
import           Network.Ethereum.Account        as Account
import           Network.Ethereum.Api.Provider   (Web3, runWeb3)
import           Network.Ethereum.Contract.Event (EventAction (..), event)
import           Network.Ethereum.Unit           as Unit
