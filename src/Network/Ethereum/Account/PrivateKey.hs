{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}

-- |
-- Module      :  Network.Ethereum.Account.PrivateKey
-- Copyright   :  Alexander Krupenkin 2018
--                Roy Blankman 2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--

module Network.Ethereum.Account.PrivateKey (
    PrivateKey(..)
  , PrivateKeyAccount
) where

import           Control.Monad.State.Strict        (get, runStateT)
import           Control.Monad.Trans               (lift)
import           Crypto.Secp256k1                  (CompactRecSig (..), SecKey,
                                                    derivePubKey)
import           Data.ByteArray                    (convert)
import           Data.ByteString                   (ByteString)
import           Data.ByteString.Short             (fromShort)
import           Data.Default                      (Default (..))
import           Data.Maybe                        (fromJust, fromMaybe)
import           Data.Monoid                       (mempty)
import           Data.Proxy                        (Proxy (..))
import           Data.RLP                          (packRLP, rlpEncode)

import           Crypto.Ethereum                   (ecsign)
import           Data.HexString                    (HexString, toBytes)
import           Data.Solidity.Abi.Codec           (decode, encode)
import           Data.Solidity.Prim.Address        (fromPubKey, toHexString)
import           Network.Ethereum.Account.Class    (Account (..))
import           Network.Ethereum.Account.Internal (AccountT (..),
                                                    CallParam (..),
                                                    defaultCallParam, getCall,
                                                    getReceipt)
import qualified Network.Ethereum.Api.Eth          as Eth (call, estimateGas,
                                                           getTransactionCount,
                                                           sendRawTransaction)
import           Network.Ethereum.Api.Types        (Call (..), unQuantity)
import           Network.Ethereum.Chain            (foundation)
import           Network.Ethereum.Contract.Method  (selector)
import           Network.Ethereum.Unit             (Shannon, toWei)

data PrivateKey = PrivateKey
    { privateKey      :: !SecKey
    , privateKeyChain :: !Integer
    } deriving (Eq, Show)

instance Default PrivateKey where
    def = PrivateKey "" foundation

type PrivateKeyAccount = AccountT PrivateKey

instance Account PrivateKey PrivateKeyAccount where
    withAccount a =
        fmap fst . flip runStateT (defaultCallParam a) . runAccountT

    send (args :: a) = do
        CallParam{..} <- get
        c <- getCall

        let dat     = selector (Proxy :: Proxy a) <> encode args
            address = fromPubKey (derivePubKey $ privateKey _account)

        nonce <- lift $ Eth.getTransactionCount address _block
        let params = c { callFrom  = Just address
                       , callNonce = Just nonce
                       , callData  = Just $ convert dat }

        gasLimit <- lift $ Eth.estimateGas params
        let params' = params { callGas = Just gasLimit }

        let signed = signTransaction params' (privateKeyChain _account) (privateKey _account)
        lift $ getReceipt =<< Eth.sendRawTransaction signed

    call (args :: a) = do
        CallParam{..} <- get
        c <- getCall
        let dat = selector (Proxy :: Proxy a) <> encode args
            address = fromPubKey (derivePubKey $ privateKey _account)
            params = c { callFrom = Just address, callData = Just $ convert dat }

        res <- lift $ Eth.call params _block
        case decode res of
            Right r -> return r
            Left e  -> fail e

encodeTransaction :: Call
                  -> Either Integer (Integer, ByteString, ByteString)
                  -> HexString
encodeTransaction Call{..} vrs = do
    let (to       :: ByteString) = fromMaybe mempty (toBytes . toHexString <$> callTo)
        (value    :: Integer)    = unQuantity $ fromJust callValue
        (nonce    :: Integer)    = unQuantity $ fromJust callNonce
        (gasPrice :: Integer)    = fromMaybe defaultGasPrice $ fmap unQuantity callGasPrice
        (gasLimit :: Integer)    = unQuantity $ fromJust callGas
        (input    :: ByteString) = convert $ fromMaybe mempty callData

    rlp $ case vrs of
        -- Unsigned transaction by EIP155
        Left chain_id   -> (nonce, gasPrice, gasLimit, to, value, input, chain_id, mempty, mempty)
        -- Signed transaction
        Right (v, r, s) -> (nonce, gasPrice, gasLimit, to, value, input, v, s, r)
  where
    rlp = convert . packRLP . rlpEncode
    defaultGasPrice = toWei (5 :: Shannon)

signTransaction :: Call
                -> Integer
                -> SecKey
                -> HexString
signTransaction c i key = encodeTransaction c $ Right (v', r, s)
  where
    unsigned = encodeTransaction c (Left i)
    recSig = ecsign key unsigned
    v  = fromIntegral $ getCompactRecSigV recSig
    r  = fromShort $ getCompactRecSigR recSig
    s  = fromShort $ getCompactRecSigS recSig
    v' = v + 35 + 2 * i  -- Improved 'v' according to EIP155
