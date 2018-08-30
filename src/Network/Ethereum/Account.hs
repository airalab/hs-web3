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
import           Network.Ethereum.Contract.Method (Method)
import           Network.JsonRpc.TinyClient       (JsonRpcM)

class Account a t | t -> a where
    withAccount :: JsonRpcM m
                => a
                -> t m b
                -> m b

    send :: (JsonRpcM m, Method b)
         => b
         -> t m HexString

    call :: (JsonRpcM m, Method b)
         => b
         -> t m HexString
