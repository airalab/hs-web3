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
import Control.Concurrent (ThreadId, threadDelay, forkIO)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy (toStrict)
import qualified Data.Text as T
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))

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
    event :: Address -> (a -> IO EventAction) -> Web3 ThreadId
    event = _event

_event :: Event a => Address -> (a -> IO EventAction) -> Web3 ThreadId
_event a f = do
    fid <- let ftyp = snd $ let x = undefined :: Event a => a
                            in  (f x, x)
           in  eth_newFilter (eventFilter ftyp a)

    cfg <- ask
    liftIO $ forkIO $
        let loop = do threadDelay 1000000
                      res <- runWeb3' cfg (eth_getFilterChanges fid)
                      case res of
                          Left e -> print e
                          Right [] -> loop
                          Right changes -> do
                              acts <- mapM f $
                                  catMaybes $ fmap parseChange changes
                              if any (== TerminateEvent) acts
                              then return ()
                              else loop
        in do loop
              runWeb3' cfg (eth_uninstallFilter fid)
              return ()
  where
    prepareTopics = fmap (T.drop 2) . drop 1
    parseChange c = fromData $
        T.append (T.concat (prepareTopics $ changeTopics c))
                 (T.drop 2 $ changeData c)

-- | Contract method caller
class ABIEncoding a => Method a where
    -- | Send a transaction for given contract 'Address', value and input data
    sendTx :: Unit b => Address -> b -> a -> Web3 TxHash
    sendTx = _sendTransaction

    -- | Constant call given contract 'Address' in mode and given input data
    call :: ABIEncoding b => Address -> CallMode -> a -> Web3 b
    call = _call

_sendTransaction :: (Method a, Unit b)
                 => Address -> b -> a -> Web3 TxHash
_sendTransaction to value dat = do
    primeAddress <- head <$> eth_accounts
    eth_sendTransaction (txdata primeAddress $ Just $ toData dat)
  where txdata from = Call (Just from) to Nothing Nothing (Just $ toWeiText value)
        toWeiText = ("0x" <>) . toStrict . B.toLazyText . B.hexadecimal . toWei

-- TODO: Correct dynamic type parsing
_call :: (Method a, ABIEncoding b)
      => Address -> CallMode -> a -> Web3 b
_call to mode dat = do
    res <- eth_call txdata mode
    case fromData (T.drop 2 res) of
        Nothing -> fail $
            "Unable to parse result on `" ++ T.unpack res ++ "`"
        Just x -> return x
  where
    txdata = Call Nothing to Nothing Nothing Nothing (Just (toData dat))

-- | Zero value is used to send transaction without money
nopay :: Wei
{-# INLINE nopay #-}
nopay = 0
