{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}

-- |
-- Module      :  Network.Ethereum.Account.Default
-- Copyright   :  Alexander Krupenkin 2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--

module Network.Ethereum.Account.Default where

import           Control.Monad.Catch               (throwM)
import           Control.Monad.State.Strict        (get, runStateT)
import           Control.Monad.Trans               (MonadTrans (..))
import qualified Data.ByteArray                    as BA (convert)
import           Data.Proxy                        (Proxy (..))

import           Data.Solidity.Abi.Codec           (decode, encode)
import           Network.Ethereum.Account.Class    (Account (..))
import           Network.Ethereum.Account.Internal (AccountT (..),
                                                    CallParam (..),
                                                    defaultCallParam, getCall,
                                                    getReceipt)
import qualified Network.Ethereum.Api.Eth          as Eth (accounts, call,
                                                           sendTransaction)
import           Network.Ethereum.Api.Provider     (Web3Error (ParserFail))
import           Network.Ethereum.Api.Types        (Call (callData, callFrom))
import           Network.Ethereum.Contract.Method  (Method (..))

type DefaultAccount = AccountT ()

instance Account () DefaultAccount where
    withAccount _ =
        fmap fst . flip runStateT (defaultCallParam ()) . runAccountT

    send (args :: a) = do
        c <- getCall
        lift $ do
            let dat = selector (Proxy :: Proxy a) <> encode args
                params = c { callData = Just $ BA.convert dat }
            accounts <- Eth.accounts
            tx <- Eth.sendTransaction $
                case accounts of
                    (a : _) -> params { callFrom = Just a }
                    _       -> params
            getReceipt tx

    call (args :: a) = do
        c <- getCall
        let dat    = selector (Proxy :: Proxy a) <> encode args
            params = c { callData = Just $ BA.convert dat }
        s <- get
        case s of
            CallParam _ _ _ _ block _ -> do
                res <- lift $ do
                    accounts <- Eth.accounts
                    flip Eth.call block $ case accounts of
                        (a : _) -> params { callFrom = Just a }
                        _       -> params
                case decode res of
                    Right r -> return r
                    Left e  -> lift $ throwM (ParserFail e)
