-- |
-- Module      :  Network.Polkadot.Extrinsic
-- Copyright   :  Aleksandr Krupenkin 2016-2021
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
  , sign_extrinsic
  , new_extra'
  , new_extra
  , new_era
  ) where

import           Codec.Scale                                                   (Compact (..))
import           Data.Maybe                                                    (fromJust)
import           Data.Text.Encoding                                            (decodeUtf8)
import           Network.JsonRpc.TinyClient                                    (JsonRpc)

import           Network.Polkadot.Account                                      (to_ss58check)
import           Network.Polkadot.Extrinsic.Era                                (Era (..))
import           Network.Polkadot.Extrinsic.SignedExtension.System
import           Network.Polkadot.Extrinsic.SignedExtension.TransactionPayment
import           Network.Polkadot.Extrinsic.Unchecked                          (UncheckedExtrinsic,
                                                                                sign_extrinsic)
import           Network.Polkadot.Primitives                                   (AccountId,
                                                                                Balance,
                                                                                Index,
                                                                                MultiAddress,
                                                                                MultiSignature)
import           Network.Polkadot.Rpc.Account                                  (nextIndex)
import           Network.Polkadot.Rpc.Chain                                    (getHeader)
import           Network.Polkadot.Rpc.Types                                    (Header (headerNumber))

-- | Default Polkadot compatible extrinsic type.
type Extrinsic a = UncheckedExtrinsic a MultiAddress MultiSignature SignedExtra

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

new_extra :: JsonRpc m
          => AccountId
          -- ^ Transaction sender address.
          -> Balance
          -- ^ Transaction tips, or set zero for no tips.
          -> m SignedExtra
          -- ^ Returns Polkadot transaction extra.
new_extra account_id tip = do
    nonce <- fromIntegral <$> nextIndex ss58account
    era <- new_era
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
new_era :: JsonRpc m => m Era
new_era = do
    blockNumber <- (headerNumber . fromJust) <$> getHeader Nothing
    return $ MortalEra 2048 $ fromIntegral (blockNumber - 1)
