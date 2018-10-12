{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import           Data.Maybe                        (fromMaybe)
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
import qualified Network.Ethereum.Api.Eth          as Eth (call,
                                                           getTransactionCount,
                                                           sendRawTransaction)
import           Network.Ethereum.Api.Types        (Call (..), unQuantity)
import           Network.Ethereum.Contract.Method  (selector)

data PrivateKey = PrivateKey
    { privateKey :: SecKey
    , chainId    :: Integer
    }
  deriving (Eq, Show)

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
        let params  = c { callFrom  = Just address
                        , callNonce = Just nonce
                        , callData  = Just $ convert dat }
            signed  = signTransaction params (chainId _account) (privateKey _account)

        lift $ case signed of
            Just params' -> getReceipt =<< Eth.sendRawTransaction params'
            Nothing -> fail $ "Unable to sign transaction: " ++ show params

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
                  -> Maybe HexString
encodeTransaction Call{..} vrs = do
    (nonce    :: Integer)    <- unQuantity <$> callNonce
    (gasPrice :: Integer)    <- unQuantity <$> callGasPrice
    (gasLimit :: Integer)    <- unQuantity <$> callGas
    (to       :: ByteString) <- toBytes . toHexString <$> callTo
    (value    :: Integer)    <- unQuantity <$> callValue
    let (dat :: ByteString)   = fromMaybe mempty (convert <$> callData)

    return . rlp $ case vrs of
        -- Unsigned transaction by EIP155
        Left chain_id   -> (nonce, gasPrice, gasLimit, to, value, dat, chain_id, mempty, mempty)
        -- Signed transaction
        Right (v, r, s) -> (nonce, gasPrice, gasLimit, to, value, dat, v, s, r)
  where
    rlp = convert . packRLP . rlpEncode

signTransaction :: Call
                -> Integer
                -> SecKey
                -> Maybe HexString
signTransaction c i key = do
    unsigned <- encodeTransaction c (Left i)
    let recSig = ecsign key unsigned
        v = fromIntegral $ getCompactRecSigV recSig
        r = fromShort $ getCompactRecSigR recSig
        s = fromShort $ getCompactRecSigS recSig
    encodeTransaction c $ Right ((toEthV v), r, s)
  where
    toEthV v = v + 35 + 2 * i
    {-# INLINE toEthV #-}
