{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      :  Network.Ethereum.Account.Safe
-- Copyright   :  Aleksandr Krupenkin 2016-2021
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- Safe sending of Ethereum transaction.
--

module Network.Ethereum.Account.Safe where

import           Control.Concurrent                (threadDelay)
import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.Trans               (lift)
import           Data.ByteArray.HexString          (HexString)

import           Network.Ethereum.Account.Class    (Account (send))
import           Network.Ethereum.Account.Internal (updateReceipt)
import qualified Network.Ethereum.Api.Eth          as Eth
import           Network.Ethereum.Api.Types        (TxReceipt (receiptBlockNumber))
import           Network.Ethereum.Contract.Method  (Method)
import           Network.JsonRpc.TinyClient        (JsonRpc)

-- | Safe version of 'send' function of 'Account' typeclass
-- Waiting for some blocks of transaction confirmation before return
safeSend :: (Account p t, JsonRpc m, Method args, Monad (t m))
         => Integer
         -- ^ Confirmation in blocks
         -> args
         -- ^ Contract method arguments
         -> t m (Either HexString TxReceipt)
         -- ^ Receipt of the sent transaction, or transaction data in case of a timeout
safeSend b a = lift . withReceipt waiting =<< send a
  where
    withReceipt f receiptOrTx =
        case receiptOrTx of
            Left tx -> return $ Left tx
            Right receipt -> Right <$> f receipt

    waiting receipt =
        case receiptBlockNumber receipt of
            Nothing -> do
                liftIO $ threadDelay 1000000
                waiting =<< updateReceipt receipt
            Just bn -> do
                current <- Eth.blockNumber
                if current - bn >= fromInteger b
                    then return receipt
                    else do liftIO $ threadDelay 1000000
                            waiting receipt

-- | Count block confirmation to keep secure
-- According to Vitalik post
-- https://blog.ethereum.org/2015/09/14/on-slow-and-fast-block-times/
safeConfirmations :: Integer
safeConfirmations = 10
