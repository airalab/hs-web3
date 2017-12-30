{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}

-- |
-- Module      :  Network.Ethereum.Web3.Contract
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Ethereum contract generalized interface, e.g. 'event' function
-- catch all event depend by given callback function type.
--
-- @
-- runWeb3 $ do
--     event "0x..." $ \(MyEvent a b c) ->
--         liftIO $ print (a + b * c))
-- @
--
-- In other case 'call' function used for constant calls (without
-- transaction creation and change state), and 'sendTx' function
-- like a 'call' but return no contract method return but created
-- transaction hash.
--
-- @
-- runweb3 $ do
--   x  <- call "0x.." Latest MySelector
--   tx <- sendTx "0x.." nopay $ MySelector2 (x + 2)
-- @
--
module Network.Ethereum.Web3.Contract (
    EventAction(..)
  , TxMethod(..)
  , CallMethod(..)
  , Event(..)
  , event
  , NoMethod(..)
  , nopay
  ) where

import           Control.Concurrent                     (ThreadId, threadDelay)
import           Control.Exception                      (throwIO)
import           Control.Monad                          (forM, when)
import           Control.Monad.IO.Class                 (liftIO)
import           Control.Monad.Trans.Reader             (ReaderT (..))
import           Data.Maybe                             (listToMaybe, mapMaybe)
import           Data.Monoid                            ((<>))
import           Data.Proxy                             (Proxy (..))
import qualified Data.Text                              as T
import           Data.Text.Lazy                         (toStrict)
import qualified Data.Text.Lazy.Builder                 as B
import qualified Data.Text.Lazy.Builder.Int             as B
import           Generics.SOP

import           Network.Ethereum.Unit
import           Network.Ethereum.Web3.Address
import           Network.Ethereum.Web3.Encoding
import           Network.Ethereum.Web3.Encoding.Event
import           Network.Ethereum.Web3.Encoding.Generic
import qualified Network.Ethereum.Web3.Eth              as Eth
import           Network.Ethereum.Web3.Provider
import           Network.Ethereum.Web3.Types

-- | Event callback control response
data EventAction = ContinueEvent
                 -- ^ Continue to listen events
                 | TerminateEvent
                 -- ^ Terminate event listener
  deriving (Show, Eq)

-- | Contract event listener
class Event e where
    -- | Event filter structure used by low-level subscription methods
    eventFilter :: Proxy e -> Address -> Filter

event :: forall p e i ni.
          ( Provider p
         , Event e
         , DecodeEvent i ni e
         )
       => Address
       -> (e -> ReaderT Change (Web3 p) EventAction)
       -> Web3 p ThreadId
event a f = do
    fid <- Eth.newFilter (eventFilter (Proxy :: Proxy e) a)
    forkWeb3 $
        let loop = do liftIO (threadDelay 1000000)
                      changes <- Eth.getFilterChanges fid
                      acts <- forM (mapMaybe pairChange changes) $ \(changeEvent, changeWithMeta) ->
                        runReaderT (f changeEvent) changeWithMeta
                      when (TerminateEvent `notElem` acts) loop
        in do loop
              Eth.uninstallFilter fid
              return ()
  where
    prepareTopics = fmap (T.drop 2) . drop 1
    pairChange :: DecodeEvent i ni e => Change -> Maybe (e, Change)
    pairChange changeWithMeta = do
      changeEvent <- decodeEvent changeWithMeta
      return (changeEvent, changeWithMeta)

-- | Contract method caller
class TxMethod a where
    -- | Send a transaction for given contract 'Address', value and input data
    sendTx :: forall p .
              Provider p
           => Call
           -- ^ Call configuration
           -> a
           -- ^ method data
           -> Web3 p TxHash
           -- ^ 'Web3' wrapped result

class CallMethod a b where
    -- | Constant call given contract 'Address' in mode and given input data
    call :: forall p .
            Provider p
         => Call
         -- ^ Call configuration
         -> DefaultBlock
         -- ^ State mode for constant call (latest or pending)
         -> a
         -- ^ Method data
         -> Web3 p b
         -- ^ 'Web3' wrapped result

instance ( Generic a
         , GenericABIEncode (Rep a)
         ) => TxMethod a where
  sendTx call dat = Eth.sendTransaction (call { callData = Just $ genericToData dat })

instance ( Generic a
         , GenericABIEncode (Rep a)
         , Generic b
         , GenericABIDecode (Rep b)
         ) => CallMethod a b where
  call call mode dat = do
    res <- Eth.call (call { callData = Just $ genericToData dat }) mode
    case genericFromData (T.drop 2 res) of
        Nothing -> liftIO $ throwIO $ ParserFail $
            "Unable to parse result on `" ++ T.unpack res
            ++ "` from `" ++ show (callTo call) ++ "`"
        Just x -> return x

-- | Zero value is used to send transaction without money
nopay :: Wei
{-# INLINE nopay #-}
nopay = 0

-- | Dummy method for sending transaction without method call
data NoMethod = NoMethod

instance ABIEncode NoMethod where
    toDataBuilder  = const ""

instance ABIDecode NoMethod where
    fromDataParser = return NoMethod

instance CallMethod NoMethod b where
    call = undefined

instance TxMethod NoMethod where
    sendTx = undefined
