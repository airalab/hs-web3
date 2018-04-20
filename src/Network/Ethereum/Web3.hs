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

module Network.Ethereum.Web3 (

  -- ** Web3 monad as base of any Ethereum communication
    Web3
  , runWeb3

  -- ** Ethereum data types
  , Address
  , Bytes
  , BytesN
  , IntN
  , UIntN
  , ListN

  -- ** Ethereum transactions
  , sendTx

  -- ** Ethereum metric unit system
  , module Network.Ethereum.Unit

  ) where

import           Network.Ethereum.ABI.Prim.Address (Address)
import           Network.Ethereum.ABI.Prim.Bool    ()
import           Network.Ethereum.ABI.Prim.Bytes   (Bytes, BytesN)
import           Network.Ethereum.ABI.Prim.Int     (IntN, UIntN)
import           Network.Ethereum.ABI.Prim.List    (ListN)
import           Network.Ethereum.ABI.Prim.String  ()
import           Network.Ethereum.Contract.Method  (sendTx)
import           Network.Ethereum.Unit
import           Network.Ethereum.Web3.Provider    (Web3, runWeb3)
