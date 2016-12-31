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
-- runWeb3 $ event "0x..." (\(MyEvent a b c) -> print (a + b * c))
-- @
--
-- In other case 'call' function used for constant calls (without
-- transaction creation and change state), and 'sendTx' function
-- like a 'call' but return no contract method return but created
-- transaction hash.
--
-- @
-- runweb3 $ do
--   x <- call "0x.." Latest MySelector
--   tx <- sendTx "0x.." nopay $ MySelector2 (x + 2)
-- @
--
module Network.Ethereum.Web3.Contract (
    EventAction(..)
  , Method(..)
  , Event(..)
  , nopay
  ) where

import qualified Data.Text.Lazy.Builder.Int as B
import qualified Data.Text.Lazy.Builder     as B
import Control.Concurrent (ThreadId, threadDelay)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (throwIO)
import Data.Text.Lazy (toStrict)
import qualified Data.Text as T
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))

import Network.Ethereum.Web3.Provider
import Network.Ethereum.Web3.Encoding
import Network.Ethereum.Web3.Address
import Network.Ethereum.Web3.Types
import Network.Ethereum.Web3.Api
import Network.Ethereum.Unit

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
          -> (a -> IO EventAction)
          -- ^ 'Event' handler
          -> Web3 p ThreadId
          -- ^ 'Web3' wrapped event handler spawn ident
    event = _event

_event :: (Provider p, Event a)
       => Address
       -> (a -> IO EventAction)
       -> Web3 p ThreadId
_event a f = do
    fid <- let ftyp = snd $ let x = undefined :: Event a => a
                            in  (f x, x)
           in  eth_newFilter (eventFilter ftyp a)

    forkWeb3 $
        let loop = do liftIO (threadDelay 1000000)
                      changes <- fmap parseChange <$> eth_getFilterChanges fid
                      acts <- mapM (liftIO . f) (catMaybes changes)
                      if any (== TerminateEvent) acts
                      then return ()
                      else loop
        in do loop
              eth_uninstallFilter fid
              return ()
  where
    prepareTopics = fmap (T.drop 2) . drop 1
    parseChange c = fromData $
        T.append (T.concat (prepareTopics $ changeTopics c))
                 (T.drop 2 $ changeData c)

-- | Contract method caller
class ABIEncoding a => Method a where
    -- | Send a transaction for given contract 'Address', value and input data
    sendTx :: (Provider p, Unit b)
           => Address
           -- ^ Contract address
           -> b
           -- ^ Payment value (set 'nopay' to empty value)
           -> a
           -- ^ Method data
           -> Web3 p TxHash
           -- ^ 'Web3' wrapped result
    sendTx = _sendTransaction

    -- | Constant call given contract 'Address' in mode and given input data
    call :: (Provider p, ABIEncoding b)
         => Address
         -- ^ Contract address
         -> CallMode
         -- ^ State mode for constant call (latest or pending)
         -> a
         -- ^ Method data
         -> Web3 p b
         -- ^ 'Web3' wrapped result
    call = _call

_sendTransaction :: (Provider p, Method a, Unit b)
                 => Address -> b -> a -> Web3 p TxHash
_sendTransaction to value dat = do
    primeAddress <- head <$> eth_accounts
    eth_sendTransaction (txdata primeAddress $ Just $ toData dat)
  where txdata from = Call (Just from) to Nothing Nothing (Just $ toWeiText value)
        toWeiText = ("0x" <>) . toStrict . B.toLazyText . B.hexadecimal . toWei

_call :: (Provider p, Method a, ABIEncoding b)
      => Address -> CallMode -> a -> Web3 p b
_call to mode dat = do
    res <- eth_call txdata mode
    case fromData (T.drop 2 res) of
        Nothing -> liftIO $ throwIO $ ParserFail $
            "Unable to parse result on `" ++ T.unpack res
            ++ "` from `" ++ show to ++ "`"
        Just x -> return x
  where
    txdata = Call Nothing to Nothing Nothing Nothing (Just (toData dat))

-- | Zero value is used to send transaction without money
nopay :: Wei
{-# INLINE nopay #-}
nopay = 0
