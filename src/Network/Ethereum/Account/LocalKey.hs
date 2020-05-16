{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- |
-- Module      :  Network.Ethereum.Account.LocalKey
-- Copyright   :  Alexander Krupenkin 2018
--                Roy Blankman 2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- Using ECC for singing transactions locally, e.g. out of Ethereum node.
-- Transaction will send using 'eth_sendRawTransacion' JSON-RPC method.
--

module Network.Ethereum.Account.LocalKey where

import           Control.Exception                 (TypeError (..))
import           Control.Monad.Catch               (throwM)
import           Control.Monad.State.Strict        (get, runStateT)
import           Control.Monad.Trans               (lift)
import           Crypto.PubKey.ECC.ECDSA           (PrivateKey)
import           Data.ByteArray                    (convert)
import           Data.ByteString                   (empty)
import           Data.Default                      (Default (..))
import           Data.Monoid                       ((<>))
import           Data.Proxy                        (Proxy (..))

import           Crypto.Ethereum                   (derivePubKey, importKey)
import           Crypto.Ethereum.Signature         (signTransaction)
import           Data.Solidity.Abi.Codec           (decode, encode)
import           Data.Solidity.Prim.Address        (fromPubKey)
import           Network.Ethereum.Account.Class    (Account (..))
import           Network.Ethereum.Account.Internal (AccountT (..),
                                                    CallParam (..),
                                                    defaultCallParam, getCall,
                                                    getReceipt)
import qualified Network.Ethereum.Api.Eth          as Eth (call, estimateGas,
                                                           getTransactionCount,
                                                           sendRawTransaction)
import           Network.Ethereum.Api.Types        (Call (..))
import           Network.Ethereum.Chain            (foundation)
import           Network.Ethereum.Contract.Method  (selector)
import           Network.Ethereum.Transaction      (encodeTransaction)

-- | Local EOA params
data LocalKey = LocalKey
    { localKeyPrivate :: !PrivateKey
    , localKeyChainId :: !Integer
    }
    deriving (Eq, Show)

instance Default LocalKey where
    def = LocalKey (importKey empty) foundation

type LocalKeyAccount = AccountT LocalKey

instance Account LocalKey LocalKeyAccount where
    withAccount a =
        fmap fst . flip runStateT (defaultCallParam a) . runAccountT

    send (args :: a) = do
        CallParam{..} <- get
        c <- getCall

        let dat     = selector (Proxy :: Proxy a) <> encode args
            address = fromPubKey (derivePubKey $ localKeyPrivate _account)

        nonce <- lift $ Eth.getTransactionCount address _block
        let params = c { callFrom  = Just address
                       , callNonce = Just nonce
                       , callData  = Just $ convert dat }

        params' <- case callGas params of
            Just _  -> return params
            Nothing -> do
                gasLimit <- lift $ Eth.estimateGas params
                return $ params { callGas = Just gasLimit }

        let packer = encodeTransaction params' (localKeyChainId _account)
            signed = signTransaction packer (localKeyPrivate _account)
        lift $ getReceipt =<< Eth.sendRawTransaction signed

    call (args :: a) = do
        CallParam{..} <- get
        c <- getCall
        let dat = selector (Proxy :: Proxy a) <> encode args
            address = fromPubKey (derivePubKey $ localKeyPrivate _account)
            params = c { callFrom = Just address, callData = Just $ convert dat }

        res <- lift $ Eth.call params _block
        case decode res of
            Right r -> return r
            Left e  -> lift (throwM $ TypeError e)
