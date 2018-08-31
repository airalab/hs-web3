{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

-- |
-- Module      :  Network.Ethereum.Account
-- Copyright   :  Alexander Krupenkin 2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
--
--

module Network.Ethereum.Account where

import           Data.HexString                   (HexString)
import           Data.Solidity.Abi                (AbiGet)
import           Network.Ethereum.Api.Types       (TxReceipt)
import           Network.Ethereum.Contract.Method (Method)
import           Network.JsonRpc.TinyClient       (JsonRpcM)

-- |
class Account a t | t -> a where
    -- | Run account monad with given account credentials
    withAccount :: JsonRpcM m
                => a
                -- ^ Account credentials like password or private key
                -> t m b
                -- ^ Monad transformer that handle account oriented rpc communication
                -> m b
                -- ^ Json-rpc monad

    -- | Send transaction like a 'write' command
    send :: (JsonRpcM m, Method args)
         => args
         -- ^ Function arguments encoded as 'Method' call
         -> t m TxReceipt
         -- ^ Receipt of sended transaction

    -- | Fast version of 'send' function excluding gas estimation and receipt waiting
    send' :: (JsonRpcM m, Method args)
          => args
          -- ^ Function arguments encoded as 'Method' call
          -> t m HexString
          -- ^ Hash of sended transaction

    -- | Call constant function like a 'read' command
    call :: (JsonRpcM m, Method args, AbiGet result)
         => args
         -- ^ Function arguments encoded as 'Method' call
         -> t m result
         -- ^ Decoded result of function call
