{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}

-- |
-- Module      :  Network.Ethereum.Account.Internal
-- Copyright   :  Aleksandr Krupenkin 2016-2021
-- License     :  Apache-2.0
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
import           Data.Either                    (fromRight)
import           Data.Maybe                     (fromMaybe)
import           Lens.Micro                     (Lens', lens)

import           Data.ByteArray.HexString       (HexString)
import           Data.Solidity.Prim             (Address)
import           Network.Ethereum.Account.Class (Account)
import qualified Network.Ethereum.Api.Eth       as Eth (getTransactionReceipt)
import           Network.Ethereum.Api.Types     (Call (..),
                                                 DefaultBlock (Latest),
                                                 TxReceipt (receiptTransactionHash))
import           Network.Ethereum.Unit          (Unit (..))
import           Network.JsonRpc.TinyClient     (JsonRpc)

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
    , _timeout  :: Maybe Int
    -- ^ Transaction timeout in microseconds
    } deriving Eq

-- | Transaction recipient lens
to :: Lens' (CallParam p) Address
to = lens (fromMaybe def . _to) $ \a b -> a { _to = Just b }

-- | Transaction value lens
value :: Unit value => Lens' (CallParam p) value
value = lens (fromWei . _value) $ \a b -> a { _value = toWei b }

-- | Transaction gas limit lens
gasLimit :: Lens' (CallParam p) Integer
gasLimit = lens (fromMaybe def . _gasLimit) $ \a b -> a { _gasLimit = Just b }

-- | Transaction gas price lens
gasPrice :: Unit gasprice => Lens' (CallParam p) gasprice
gasPrice = lens (fromWei . fromMaybe def . _gasPrice) $ \a b -> a { _gasPrice = Just (toWei b) }

-- | Call execution block lens
block :: Lens' (CallParam p) DefaultBlock
block = lens _block $ \a b -> a { _block = b }

-- | EOA params lens
account :: Lens' (CallParam p) p
account = lens _account $ \a b -> a { _account = b }

-- | Transaction timeout lens
timeout :: Lens' (CallParam p) (Maybe Int)
timeout = lens _timeout $ \a b -> a { _timeout = b }

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
defaultCallParam acc = CallParam def 0 Nothing Nothing Latest acc Nothing

getCall :: MonadState (CallParam p) m => m Call
getCall = do
    CallParam{..} <- get
    return $ def { callTo       = _to
                 , callValue    = Just $ fromInteger _value
                 , callGas      = fromInteger <$> _gasLimit
                 , callGasPrice = fromInteger <$> _gasPrice
                 }

getReceipt :: JsonRpc m => Maybe Int -> HexString -> m (Either HexString TxReceipt)
getReceipt mbtimeout tx = do
    mbreceipt <- Eth.getTransactionReceipt tx
    case mbreceipt of
        Just receipt -> return $ Right receipt
        Nothing -> case mbtimeout of
            Just us
                | us > 0 -> retry $ Just $ us - 100000
                | otherwise -> return $ Left tx
            Nothing -> retry Nothing
    where
        retry mbtimeout' = do
            liftIO $ threadDelay 100000
            getReceipt mbtimeout' tx

getReceipt' :: JsonRpc m => HexString -> m TxReceipt
getReceipt' = fmap (fromRight undefined) . getReceipt Nothing

updateReceipt :: JsonRpc m => TxReceipt -> m TxReceipt
{-# INLINE updateReceipt #-}
-- No timeout, because we update the receipt of an already processed transaction.
updateReceipt = getReceipt' . receiptTransactionHash
