-- Module      :  Network.Ethereum.Web3
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unknown
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
  -- ** Web3 monad and service provider
    Web3
  , Provider(..)
  , DefaultProvider
  , Web3Error(..)
  , forkWeb3
  , runWeb3'
  , runWeb3
  -- ** Contract actions
  , EventAction(..)
  , Event(..)
  , event
  , event'
  , eventMany'
  , Method(..)
  , sendTx
  , call
  , NoMethod(..)
  , nopay
  -- ** Ethereum data types
  , BytesN(..)
  , BytesD(..)
  , IntN(..)
  , intNFromInteger
  , UIntN(..)
  , uIntNFromInteger
  , Address
  , Vector
  -- ** Ethereum unit conversion utils
  , module Network.Ethereum.Unit
  , IndexedEvent(..)
  ) where

import           Network.Ethereum.Unit
import           Network.Ethereum.Web3.Address
import           Network.Ethereum.Web3.Contract
import           Network.Ethereum.Web3.Encoding
import           Network.Ethereum.Web3.Encoding.Bytes
import           Network.Ethereum.Web3.Encoding.Event
import           Network.Ethereum.Web3.Encoding.Generic
import           Network.Ethereum.Web3.Encoding.Int
import           Network.Ethereum.Web3.Encoding.Vector
import           Network.Ethereum.Web3.Provider
import           Network.Ethereum.Web3.Types
