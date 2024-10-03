{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      :  Network.Polkadot.Extrinsic
-- Copyright   :  Aleksandr Krupenkin 2016-2024
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- Extrinsic is a piece of data from external world.
--

module Network.Polkadot.Extrinsic
  ( Extrinsic
  , SignedExtra
  , sign_and_send
  , mortal_max
  , new_extra'
  , new_extra
  ) where

import           Codec.Scale                                                   (Compact (..),
                                                                                Encode,
                                                                                encode)
import           Data.ByteArray.HexString                                      (HexString)
import           Data.Maybe                                                    (fromJust)
import           Data.Text.Encoding                                            (decodeUtf8)
import           Network.JsonRpc.TinyClient                                    (JsonRpc)

import           Network.Polkadot.Account                                      (AccountId,
                                                                                IdentifyAccount (..),
                                                                                Ss58Codec (to_ss58check))
import           Network.Polkadot.Crypto                                       (MultiPair (..))
import           Network.Polkadot.Extrinsic.Era                                (Era (..))
import           Network.Polkadot.Extrinsic.SignedExtension.System
import           Network.Polkadot.Extrinsic.SignedExtension.TransactionPayment
import           Network.Polkadot.Extrinsic.Unchecked                          (UncheckedExtrinsic,
                                                                                sign_extrinsic)
import           Network.Polkadot.Primitives                                   (Balance,
                                                                                Index)
import qualified Network.Polkadot.Primitives                                   as P (MultiAddress,
                                                                                     MultiSignature)
import           Network.Polkadot.Rpc.Account                                  (nextIndex)
import           Network.Polkadot.Rpc.Author                                   (submitExtrinsic)
import           Network.Polkadot.Rpc.Chain                                    (getHeader)
import           Network.Polkadot.Rpc.Types                                    (headerNumber,
                                                                                unBlockNumber)

-- | Default Polkadot compatible extrinsic type.
type Extrinsic a = UncheckedExtrinsic a P.MultiAddress P.MultiSignature SignedExtra

-- | Default Polkadot signed extra.
type SignedExtra =
  ( CheckSpecVersion
  , CheckTxVersion
  , CheckGenesis
  , CheckEra
  , CheckNonce
  , CheckWeight
  , ChargeTransactionPayment
  )

new_extra :: (Ss58Codec a, JsonRpc m)
          => a
          -- ^ Transaction sender address.
          -> Balance
          -- ^ Transaction tips, or set zero for no tips.
          -> m SignedExtra
          -- ^ Returns Polkadot transaction extra.
new_extra account_id tip = do
    nonce <- fromIntegral <$> nextIndex ss58account
    era <- mortal_max
    return $ new_extra' era nonce tip
  where
    ss58account = decodeUtf8 $ to_ss58check account_id

-- | Create signed extra from general data.
new_extra' :: Era
           -- ^ Transaction mortality.
           -> Index
           -- ^ Transaction nonce value.
           -> Balance
           -- ^ Transaction tips, or set zero for no tips.
           -> SignedExtra
           -- ^ Returns Polkadot transaction extra.
new_extra' era nonce tip =
    ( CheckSpecVersion
    , CheckTxVersion
    , CheckGenesis
    , CheckEra era
    , CheckNonce (Compact nonce)
    , CheckWeight
    , ChargeTransactionPayment (Compact tip)
    )

-- | Create a mortal 'Era' with biggest lifetime period.
--
-- Note: The assumption is runtime has `BlockHashCount` = 2400. This is common
-- for Polkadot runtimes.
mortal_max :: JsonRpc m => m Era
mortal_max = do
    current <- (unBlockNumber . headerNumber . fromJust) <$> getHeader Nothing
    return $ MortalEra 2048 $ fromIntegral (current - 1)

-- | Sign extrinsic and send it using node RPC call.
sign_and_send :: ( MultiPair pair
                 , IdentifyAccount (MultiSigner pair)
                 , Ss58Codec (AccountId (MultiSigner pair))
                 , Encode (MultiAddress pair)
                 , Encode (MultiSignature pair)
                 , Encode call
                 , JsonRpc m
                 )
              => pair
              -- ^ Sender account pair.
              -> call
              -- ^ Runtime function to call.
              -> Balance
              -- ^ Tips for speedup transaction (set zero for no boost).
              -> m HexString
              -- ^ Transaction hash.
sign_and_send pair call tip = do
    extra <- new_extra account_id tip
    xt <- sign_extrinsic pair call extra
    submitExtrinsic (encode xt)
  where
    account_id = into_account (multi_signer pair)
