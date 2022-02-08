{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- |
-- Module      :  Network.Ethereum.Account.Personal
-- Copyright   :  Aleksandr Krupenkin 2016-2021
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- Node managed unlockable account. Typically to send transaction from this account
-- password is required.
--

module Network.Ethereum.Account.Personal where

import           Control.Exception                 (TypeError (..))
import           Control.Monad.Catch               (throwM)
import           Control.Monad.State.Strict        (get, runStateT)
import           Control.Monad.Trans               (lift)
import qualified Data.ByteArray                    as BA (convert)
import           Data.Default                      (Default (..))
import           Data.Proxy                        (Proxy (..))

import           Data.Solidity.Abi.Codec           (decode, encode)
import           Data.Solidity.Prim.Address        (Address)
import           Network.Ethereum.Account.Class    (Account (..))
import           Network.Ethereum.Account.Internal (AccountT (..),
                                                    CallParam (..),
                                                    defaultCallParam, getCall,
                                                    getReceipt)
import qualified Network.Ethereum.Api.Eth          as Eth (call, estimateGas)
import           Network.Ethereum.Api.Personal     (Passphrase)
import qualified Network.Ethereum.Api.Personal     as Personal (sendTransaction)
import           Network.Ethereum.Api.Types        (Call (callData, callFrom, callGas))
import           Network.Ethereum.Contract.Method  (selector)

-- | Unlockable node managed account params
data Personal = Personal
    { personalAddress    :: !Address
    , personalPassphrase :: !Passphrase
    }
    deriving (Eq, Show)

instance Default Personal where
    def = Personal def ""

type PersonalAccount = AccountT Personal

instance Account Personal PersonalAccount where
    withAccount a =
        fmap fst . flip runStateT (defaultCallParam a) . runAccountT

    send (args :: a) = do
        CallParam{..} <- get
        c <- getCall
        lift $ do
            let dat    = selector (Proxy :: Proxy a) <> encode args
                params = c { callFrom = Just $ personalAddress _account
                           , callData = Just $ BA.convert dat }

            params' <- case callGas params of
                Just _  -> return params
                Nothing -> do
                    gasLimit <- Eth.estimateGas params
                    return $ params { callGas = Just gasLimit }

            getReceipt _timeout =<< Personal.sendTransaction params' (personalPassphrase _account)

    call (args :: a) = do
        s <- get
        case s of
            CallParam _ _ _ _ block (Personal address _) _ -> do
                c <- getCall
                let dat = selector (Proxy :: Proxy a) <> encode args
                    params = c { callFrom = Just address, callData = Just $ BA.convert dat }
                res <- lift $ Eth.call params block
                case decode res of
                    Right r -> return r
                    Left e  -> lift (throwM $ TypeError e)
