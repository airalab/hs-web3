{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}

-- |
-- Module      :  Network.Ethereum.Account.Class
-- Copyright   :  Aleksandr Krupenkin 2016-2020
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- Ethereum external owned account abstraction.
--

module Network.Ethereum.Account.Class where

import           Control.Monad.Trans              (MonadTrans)

import           Data.Solidity.Abi                (AbiGet)
import           Network.Ethereum.Api.Types       (TxReceipt)
import           Network.Ethereum.Contract.Method (Method)
import           Network.JsonRpc.TinyClient       (JsonRpc)

-- | Account is needed for sending transactions to blockchain
--
-- Typically account is provided by node. In this case node manage accounts:
-- encrypt and decrypt private keys, manipulate files etc. In other case web3
-- can derive account from private key and send to node already signed transactions.
--
class MonadTrans t => Account a t | t -> a where

    -- | Run computation with given account credentials
    withAccount :: JsonRpc m
                => a
                -- ^ Account params (like a password or private key)
                -> t m b
                -- ^ Computation that use account for sending transactions
                -> m b
                -- ^ Json-rpc monad

    -- | Send transaction to contract, like a 'write' command
    send :: (JsonRpc m, Method args)
         => args
         -- ^ Contract method arguments
         -> t m TxReceipt
         -- ^ Receipt of sended transaction

    -- | Call constant method of contract, like a 'read' command
    call :: (JsonRpc m, Method args, AbiGet result)
         => args
         -- ^ Contact method arguments
         -> t m result
         -- ^ Decoded result of method call

