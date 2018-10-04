{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

-- |
-- Module      :  Network.Ethereum.Account.Internal
-- Copyright   :  Alexander Krupenkin 2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--

module Network.Ethereum.Account.Internal where

import           Control.Concurrent             (threadDelay)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.State.Strict     (MonadState (..), StateT (..),
                                                 withStateT)
import           Control.Monad.Trans            (MonadTrans (..))
import           Data.Default                   (Default (..))
import           Data.HexString                 (HexString)
import           Data.Solidity.Prim             (Address)
import           Lens.Micro                     (Lens', lens)
import           Network.Ethereum.Account.Class (Account)
import qualified Network.Ethereum.Api.Eth       as Eth (getTransactionReceipt)
import           Network.Ethereum.Api.Types     (Call (..),
                                                 DefaultBlock (Latest),
                                                 TxReceipt (receiptTransactionHash))
import           Network.Ethereum.Unit          (Unit (toWei), Wei)
import           Network.JsonRpc.TinyClient     (JsonRpcM)

-- | Account is needed to send transactions to blockchain

-- | TODO
data CallParam p where
    CallParam :: (Unit value, Unit gasprice)
              => Address
              -- ^ Transaction recepient
              -> value
              -- ^ Transaction value
              -> Maybe Integer
              -- ^ Transaction gas limit
              -> Maybe gasprice
              -- ^ Transaction gas price
              -> DefaultBlock
              -- ^ Call block number
              -> p
              -- ^ Account params to sign transaction
              -> CallParam p

to :: Lens' (CallParam p) Address
to = lens getTo (flip setTo)
  where
    getTo = \case
        CallParam t _ _ _ _ _ -> t
    setTo t = \case
        CallParam _ a b c d e -> CallParam t a b c d e

-- | TODO
newtype AccountT p m a = AccountT
    { runAccountT :: StateT (CallParam p) m a }
  deriving (Functor, Applicative, Monad, MonadTrans)

instance Monad m => MonadState (CallParam p) (AccountT p m) where
    get = AccountT get
    put = AccountT . put

withParam :: Account p (AccountT p)
          => (CallParam p -> CallParam p)
          -> AccountT p m a
          -> AccountT p m a
{-# INLINE withParam #-}
withParam f m = AccountT $ withStateT f $ runAccountT m

defaultCallParam :: a -> CallParam a
{-# INLINE defaultCallParam #-}
defaultCallParam =
    CallParam "0x0000000000000000000000000000000000000000" (0 :: Wei) Nothing (Nothing :: Maybe Wei) Latest

getCall :: MonadState (CallParam t) m => m Call
getCall = do
    s <- get
    return $ case s of
        CallParam recipient value gas gasprice _ _ ->
            def { callTo       = Just recipient
                , callValue    = Just (fromInteger $ toWei value)
                , callGas      = fromInteger <$> gas
                , callGasPrice = (fromInteger . toWei) <$> gasprice
                }

getReceipt :: JsonRpcM m => HexString -> m TxReceipt
getReceipt tx = do
    mbreceipt <- Eth.getTransactionReceipt tx
    case mbreceipt of
        Just receipt -> return receipt
        Nothing -> do
            liftIO $ threadDelay 100000
            -- | TODO: avoid inifinite loop
            getReceipt tx

updateReceipt :: JsonRpcM m => TxReceipt -> m TxReceipt
{-# INLINE updateReceipt #-}
updateReceipt = getReceipt . receiptTransactionHash
