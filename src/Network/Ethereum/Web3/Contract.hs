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
  , Method(..)
  , Event(..)
  , NoMethod(..)
  , nopay
  ) where

import           Control.Concurrent             (ThreadId, threadDelay)
import           Control.Exception              (throwIO)
import           Control.Monad                  (forM, when)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Trans.Reader     (ReaderT (..))
import           Data.Maybe                     (listToMaybe, mapMaybe)
import           Data.Monoid                    ((<>))
import qualified Data.Text                      as T
import           Data.Text.Lazy                 (toStrict)
import qualified Data.Text.Lazy.Builder         as B
import qualified Data.Text.Lazy.Builder.Int     as B

import           Network.Ethereum.Unit
import           Network.Ethereum.Web3.Address
import           Network.Ethereum.Web3.Encoding
import qualified Network.Ethereum.Web3.Eth      as Eth
import           Network.Ethereum.Web3.Provider
import           Network.Ethereum.Web3.Types

-- | Event callback control response
data EventAction = ContinueEvent
                 -- ^ Continue to listen events
                 | TerminateEvent
                 -- ^ Terminate event listener
  deriving (Show, Eq)

-- | Contract event listener
class ABIEncoding a => Event a where
    -- | Event filter structure used by low-level subscription methods
    eventFilter :: a -> Address -> Filter

    -- | Start an event listener for given contract 'Address' and callback
    event :: Provider p
          => Address
          -- ^ Contract address
          -> (a -> ReaderT Change (Web3 p) EventAction)
          -- ^ 'Event' handler
          -> Web3 p ThreadId
          -- ^ 'Web3' wrapped event handler spawn ident
    event = _event

_event :: (Provider p, Event a)
       => Address
       -> (a -> ReaderT Change (Web3 p) EventAction)
       -> Web3 p ThreadId
_event a f = do
    fid <- let ftyp = snd $ let x = undefined :: Event a => a
                            in  (f x, x)
           in  Eth.newFilter (eventFilter ftyp a)

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
    pairChange changeWithMeta = do
      changeEvent <- fromData $
        T.append (T.concat (prepareTopics $ changeTopics changeWithMeta))
                 (T.drop 2 $ changeData changeWithMeta)
      return (changeEvent, changeWithMeta)

-- | Contract method caller
class ABIEncoding a => Method a where
    -- | Send a transaction for given contract 'Address', value and input data
    sendTx :: (Provider p)
           => Call
           -- ^ Call configuration
           -> a
           -- ^ Method data
           -> Web3 p TxHash
           -- ^ 'Web3' wrapped result
    sendTx = _sendTransaction

    -- | Constant call given contract 'Address' in mode and given input data
    call :: (Provider p, ABIEncoding b)
         => Call
         -- ^ Call configuration
         -> DefaultBlock
         -- ^ State mode for constant call (latest or pending)
         -> a
         -- ^ Method data
         -> Web3 p b
         -- ^ 'Web3' wrapped result
    call = _call

_sendTransaction :: (Provider p, Method a)
                 => Call -> a -> Web3 p TxHash
_sendTransaction call dat = Eth.sendTransaction (call { callData = Just $ toData dat })

_call :: (Provider p, Method a, ABIEncoding b)
      => Call -> DefaultBlock -> a -> Web3 p b
_call call mode dat = do
    res <- Eth.call (call { callData = Just $ toData dat }) mode
    case fromData (T.drop 2 res) of
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

instance ABIEncoding NoMethod where
    fromDataParser = return NoMethod
    toDataBuilder  = const ""

instance Method NoMethod
