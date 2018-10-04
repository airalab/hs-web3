{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}

-- |
-- Module      :  Network.Ethereum.Account.Personal
-- Copyright   :  Alexander Krupenkin 2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--

module Network.Ethereum.Account.Personal where

import           Control.Monad.State.Strict        (get, runStateT)
import           Control.Monad.Trans               (lift)
import qualified Data.ByteArray                    as BA (convert)
import           Data.Default                      (Default (..))
import           Data.Proxy                        (Proxy (..))

import           Data.Solidity.Abi.Codec           (decode, encode)
import           Data.Solidity.Prim.Address        (Address)
import           Network.Ethereum.Account.Class    (Account (..))
import           Network.Ethereum.Account.Internal (CallParam (..),
                                                    defaultCallParam)
import           Network.Ethereum.Account.Internal (AccountT (..), getCall,
                                                    getReceipt)
import qualified Network.Ethereum.Api.Eth          as Eth (call)
import           Network.Ethereum.Api.Personal     (Passphrase)
import qualified Network.Ethereum.Api.Personal     as Personal (sendTransaction)
import           Network.Ethereum.Api.Types        (Call (callData, callFrom))
import           Network.Ethereum.Contract.Method  (selector)

data Personal = Personal !Address !Passphrase
  deriving (Eq, Show)

instance Default Personal where
    def = Personal "0x0000000000000000000000000000000000000000" ""

type PersonalAccount = AccountT Personal

instance Account Personal PersonalAccount where
    withAccount a =
        fmap fst . flip runStateT (defaultCallParam a) . runAccountT

    send (args :: a) = do
        s <- get
        case s of
            CallParam _ _ _ _ _ (Personal address passphrase) -> do
                c <- getCall
                let dat    = selector (Proxy :: Proxy a) <> encode args
                    params = c { callFrom = Just address, callData = Just $ BA.convert dat }
                lift $ do
                    tx <- Personal.sendTransaction params passphrase
                    getReceipt tx

    call (args :: a) = do
        s <- get
        case s of
            CallParam _ _ _ _ block (Personal address _) -> do
                c <- getCall
                let dat = selector (Proxy :: Proxy a) <> encode args
                    params = c { callFrom = Just address, callData = Just $ BA.convert dat }
                res <- lift $ Eth.call params block
                case decode res of
                    Right r -> return r
                    Left e  -> fail e
