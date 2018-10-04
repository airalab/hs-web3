{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}

-- |
-- Module      :  Network.Ethereum.Contract
-- Copyright   :  Alexander Krupenkin 2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
--
--

module Network.Ethereum.Contract where

import           Data.Proxy                     (Proxy)
import           Data.Text                      (Text)

import           Data.HexString                 (HexString)
import           Network.Ethereum.Account.Class (Account)
import           Network.Ethereum.Api.Types     (TxReceipt)
import           Network.JsonRpc.TinyClient     (JsonRpcM)

class Contract a where
    abi :: Proxy a
        -> Text

    bytecode :: Proxy a
             -> HexString

new :: (Account p t, JsonRpcM m)
    => Proxy a
    -> t m TxReceipt
new = undefined
