{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- |
-- Module      :  Network.Ethereum.Account.Default
-- Copyright   :  Alexander Krupenkin 2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- Default node managed account (typically the first of accounts list).
--

module Network.Ethereum.Account.Default where

import           Control.Exception                 (TypeError (..))
import           Control.Monad.Catch               (throwM)
import           Control.Monad.State.Strict        (get, runStateT)
import           Control.Monad.Trans               (MonadTrans (..))
import qualified Data.ByteArray                    as BA (convert)
import           Data.Maybe                        (listToMaybe)
import           Data.Proxy                        (Proxy (..))

import           Data.Solidity.Abi.Codec           (decode, encode)
import           Network.Ethereum.Account.Class    (Account (..))
import           Network.Ethereum.Account.Internal (AccountT (..),
                                                    CallParam (..),
                                                    defaultCallParam, getCall,
                                                    getReceipt)
import qualified Network.Ethereum.Api.Eth          as Eth (accounts, call,
                                                           estimateGas,
                                                           sendTransaction)
import           Network.Ethereum.Api.Types        (Call (callData, callFrom, callGas))
import           Network.Ethereum.Contract.Method  (Method (..))

type DefaultAccount = AccountT ()

instance Account () DefaultAccount where
    withAccount _ =
        fmap fst . flip runStateT (defaultCallParam ()) . runAccountT

    send (args :: a) = do
        c <- getCall
        lift $ do
            accounts <- Eth.accounts
            let dat = selector (Proxy :: Proxy a) <> encode args
                params = c { callData = Just $ BA.convert dat
                           , callFrom = listToMaybe accounts }

            params' <- case callGas params of
                Just _  -> return params
                Nothing -> do
                    gasLimit <- Eth.estimateGas params
                    return $ params { callGas = Just gasLimit }

            getReceipt =<< Eth.sendTransaction params'

    call (args :: a) = do
        c <- getCall
        CallParam{..} <- get
        res <- lift $ do
            accounts <- Eth.accounts
            let dat    = selector (Proxy :: Proxy a) <> encode args
                params = c { callData = Just $ BA.convert dat
                           , callFrom = listToMaybe accounts }
            Eth.call params _block
        case decode res of
            Right r -> return r
            Left e  -> lift (throwM $ TypeError e)
