{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
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
  , Event(..)
  , event
  , Method(..)
  , call
  , sendTx
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
import           GHC.TypeLits
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

-- | 'event' spawns an asynchronous event filter to monitor the latest events
-- | logged by the contract at the given address for a particular event type. All
-- | events of type 'e' are composed of an indexed component 'i', and a
-- | non-indexed component 'ni'.
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


class Method a where
  -- | selector is used to compute the function selector for a given function type, defined as
  -- | the hex string representation of the first 4 bytes of the hash of the signature.
  selector :: Proxy a -> T.Text

sendTx :: ( Generic a
          , GenericABIEncode (Rep a)
          , Provider p
          , Method a
          )
       => Call
       -- ^ Call configuration
       -> a
       -- ^ method data
       -> Web3 p TxHash
sendTx call (dat :: a) =
  let sel = selector (Proxy :: Proxy a)
  in Eth.sendTransaction (call { callData = Just $ sel <> genericToData dat })

call :: ( Generic a
        , GenericABIEncode (Rep a)
        , Generic b
        , GenericABIDecode (Rep b)
        , Provider p
        , Method a
        )
     => Call
     -- ^ Call configuration
     -> DefaultBlock
     -- ^ State mode for constant call (latest or pending)
     -> a
     -- ^ Method data
     -> Web3 p b
     -- ^ 'Web3' wrapped result
call call mode (dat :: a) = do
    let sel = selector (Proxy :: Proxy a)
    res <- Eth.call (call { callData = Just $ sel <> genericToData dat }) mode
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
