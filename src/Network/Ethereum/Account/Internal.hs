{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

-- |
-- Module      :  Network.Ethereum.Account.Internal
-- Copyright   :  Alexander Krupenkin 2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- Internal types and functions of 'Account' module.
--

module Network.Ethereum.Account.Internal where

import           Control.Concurrent             (threadDelay)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.State.Strict     (MonadState (..), StateT (..),
                                                 withStateT)
import           Control.Monad.Trans            (MonadTrans (..))
import           Data.Default                   (Default (..))
import           Lens.Micro                     (Lens', lens)

import           Data.HexString                 (HexString)
import           Data.Solidity.Prim             (Address)
import           Network.Ethereum.Account.Class (Account)
import qualified Network.Ethereum.Api.Eth       as Eth (getTransactionReceipt)
import           Network.Ethereum.Api.Types     (Call (..),
                                                 DefaultBlock (Latest),
                                                 TxReceipt (receiptTransactionHash))
import           Network.Ethereum.Unit          (Unit (..))
import           Network.JsonRpc.TinyClient     (JsonRpcM)

-- | Account is needed to send transactions to blockchain

-- | Transaction parametrization data type
data CallParam p = CallParam
    { _to       :: Maybe Address
    -- ^ Transaction recepient
    , _value    :: Integer
    -- ^ Transaction value
    , _gasLimit :: Maybe Integer
    -- ^ Transaction gas limit
    , _gasPrice :: Maybe Integer
    -- ^ Transaction gas price
    , _block    :: DefaultBlock
    -- ^ Call block number
    , _account  :: p
    -- ^ Account params to sign transaction
    } deriving Eq

-- | Transaction recipient lens
to :: Lens' (CallParam p) Address
to = lens (maybe def id . _to) $ \a b -> a { _to = Just b }

-- | Transaction value lens
value :: Unit value => Lens' (CallParam p) value
value = lens (fromWei . _value) $ \a b -> a { _value = toWei b }

-- | Transaction gas limit lens
gasLimit :: Lens' (CallParam p) (Maybe Integer)
gasLimit = lens _gasLimit $ \a b -> a { _gasLimit = b }

-- | Transaction gas price lens
gasPrice :: Unit gasprice => Lens' (CallParam p) (Maybe gasprice)
gasPrice = lens (fmap fromWei . _gasPrice) $ \a b -> a { _gasPrice = toWei <$> b }

-- | Call execution block lens
block :: Lens' (CallParam p) DefaultBlock
block = lens _block $ \a b -> a { _block = b }

-- | EOA params lens
account :: Lens' (CallParam p) p
account = lens _account $ \a b -> a { _account = b }

-- | Monad transformer for sending parametrized transactions from account
newtype AccountT p m a = AccountT
    { runAccountT :: StateT (CallParam p) m a }
  deriving (Functor, Applicative, Monad, MonadTrans)

instance Monad m => MonadState (CallParam p) (AccountT p m) where
    get = AccountT get
    put = AccountT . put

-- | @withParam@ is very similar to @withStateT@ function, it's used
-- to set parameters of transaction locally and revert params after out of scope.
--
--  @
--  withAccount () $
--    withParam (to .~ tokenAddress) $
--      transfer alice 42
--  @
withParam :: Account p (AccountT p)
          => (CallParam p -> CallParam p)
          -> AccountT p m a
          -> AccountT p m a
{-# INLINE withParam #-}
withParam f m = AccountT $ withStateT f $ runAccountT m

defaultCallParam :: a -> CallParam a
{-# INLINE defaultCallParam #-}
defaultCallParam = CallParam def 0 Nothing Nothing Latest

getCall :: MonadState (CallParam p) m => m Call
getCall = do
    CallParam{..} <- get
    return $ def { callTo       = _to
                 , callValue    = Just $ fromInteger _value
                 , callGas      = fromInteger <$> _gasLimit
                 , callGasPrice = fromInteger <$> _gasPrice
                 }

getReceipt :: JsonRpcM m => HexString -> m TxReceipt
getReceipt tx = do
    mbreceipt <- Eth.getTransactionReceipt tx
    case mbreceipt of
        Just receipt -> return receipt
        Nothing -> do
            liftIO $ threadDelay 100000
            -- TODO: avoid inifinite loop
            getReceipt tx

updateReceipt :: JsonRpcM m => TxReceipt -> m TxReceipt
{-# INLINE updateReceipt #-}
updateReceipt = getReceipt . receiptTransactionHash
