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

import qualified Data.Text.Lazy.Builder.Int as B
import qualified Data.Text.Lazy.Builder     as B
import Control.Concurrent (ThreadId, threadDelay)
import Data.Maybe (mapMaybe, listToMaybe)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (throwIO)
import Data.Text.Lazy (toStrict)
import qualified Data.Text as T
import Control.Monad (when)
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
          -> (Change a -> Web3 p EventAction)
          -- ^ 'Event' handler
          -> Web3 p ThreadId
          -- ^ 'Web3' wrapped event handler spawn ident
    event = _event

_event :: (Provider p, Event a)
       => Address
       -> (Change a -> Web3 p EventAction)
       -> Web3 p ThreadId
_event a f = do
    fid <- let ftyp = snd $ let x = undefined :: Event a => Change a
                            in  (f x, x)
           in  eth_newFilter (eventFilter (changeData ftyp) a)

    forkWeb3 $
        let loop = do liftIO (threadDelay 1000000)
                      changes <- eth_getFilterChanges fid
                      acts <- mapM f (mapMaybe parseChange changes)
                      when (TerminateEvent `notElem` acts) loop
        in do loop
              eth_uninstallFilter fid
              return ()
  where
    prepareTopics = fmap (T.drop 2) . drop 1
    parseChange c = do
      a <- fromData $ T.append (T.concat (prepareTopics $ changeTopics c))
                 (T.drop 2 $ changeData c)
      return $ c { changeData = a}

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
    primeAddress <- listToMaybe <$> eth_accounts
    eth_sendTransaction (txdata primeAddress $ Just $ toData dat)
  where txdata from = Call from to (Just defaultGas) Nothing (Just $ toWeiText value)
        toWeiText   = ("0x" <>) . toStrict . B.toLazyText . B.hexadecimal . toWei
        defaultGas  = "0x2DC2DC"

_call :: (Provider p, Method a, ABIEncoding b)
      => Address -> CallMode -> a -> Web3 p b
_call to mode dat = do
    primeAddress <- listToMaybe <$> eth_accounts
    res <- eth_call (txdata primeAddress) mode
    case fromData (T.drop 2 res) of
        Nothing -> liftIO $ throwIO $ ParserFail $
            "Unable to parse result on `" ++ T.unpack res
            ++ "` from `" ++ show to ++ "`"
        Just x -> return x
  where
    txdata from = Call from to Nothing Nothing Nothing (Just (toData dat))

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
