{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ApplicativeDo  #-}

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

import           Control.Concurrent                     (threadDelay)
import           Control.Concurrent.Async                     (Async)
import           Control.Exception                      (throwIO)
import           Control.Monad                          (forM, when, void)
import           Control.Monad.IO.Class                 (liftIO)
import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.Maybe      (MaybeT (..))
import           Control.Monad.Trans.Reader             (ReaderT (..))
import           Data.Maybe                             (listToMaybe, mapMaybe)
import           Data.Machine
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

import Data.Machine.Plan



--------------------------------------------------------------------------------
-- * Event Streaming
--------------------------------------------------------------------------------

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

event :: forall p i ni e .
         ( Provider p
         , DecodeEvent i ni e
         , Event e
         )
      => Filter
      -> (e -> ReaderT Change (Web3 p) EventAction)
      -> Web3 p (Async ())
event fltr handler = forkWeb3 $ event' fltr 0 handler

event' :: forall p i ni e .
          ( Provider p
          , DecodeEvent i ni e
          , Event e
          )
       => Filter
       -> Integer
       -> (e -> ReaderT Change (Web3 p) EventAction)
       -> Web3 p ()
event' fltr window handler = do
  start <- mkBlockNumber $ filterFromBlock fltr
  let initState = FilterStreamState { fssCurrentBlock = start
                                    , fssInitialFilter = fltr
                                    , fssWindowSize = window
                                    }
  mLastProcessedFilterState <- reduceEventStream (playLogs initState) handler
  case mLastProcessedFilterState of
    Nothing -> return ()
    Just (act, lastBlock) -> when (act /= TerminateEvent) $ do
      let pollingFromBlock = lastBlock + 1
          pollTo = filterToBlock fltr
      filterId <- Eth.newFilter fltr { filterFromBlock = BlockWithNumber pollingFromBlock }
      void $ reduceEventStream (pollFilter filterId pollTo) handler

reduceEventStream :: Monad m
                  => MachineT m k [FilterChange a]
                  -> (a -> ReaderT Change m EventAction)
                  -> m (Maybe (EventAction, BlockNumber))
reduceEventStream filterChanges handler = fmap listToMaybe . runT $
       filterChanges
    ~> autoM (processChanges handler)
    ~> asParts
    ~> runWhile (\(act, _) -> act /= TerminateEvent)
    ~> final
  where
    runWhile p = repeatedly $ do
      v <- await
      if p v
        then yield v
        else yield v >> stop
    processChanges :: Monad m
                   => (a -> ReaderT Change m EventAction)
                   -> [FilterChange a]
                   -> m [(EventAction, BlockNumber)]
    processChanges handler changes = forM changes $ \FilterChange{..} -> do
                                       act <- flip runReaderT filterChangeRawChange $
                                            handler filterChangeEvent
                                       return (act, changeBlockNumber filterChangeRawChange)

data FilterChange a = FilterChange { filterChangeRawChange :: Change
                                   , filterChangeEvent     :: a
                                   }

-- | 'playLogs' streams the 'filterStream' and calls eth_getLogs on these
-- | 'Filter' objects.
playLogs :: forall p k i ni e.
            ( Provider p
            , DecodeEvent i ni e
            , Event e
            )
         => FilterStreamState
         -> MachineT (Web3 p) k [FilterChange e]
playLogs s = filterStream s
          ~> autoM Eth.getLogs
          ~> mapping mkFilterChanges

pollFilter :: forall p i ni e s k.
              ( Provider p
              , DecodeEvent i ni e
              , Event e
              )
           => FilterId
           -> DefaultBlock
           -> MachineT (Web3 p) k [FilterChange e]
pollFilter fid end = construct $ pollPlan fid end
  where
    pollPlan :: FilterId -> DefaultBlock -> PlanT k [FilterChange e] (Web3 p) ()
    pollPlan fid end = do
      bn <- lift $ Eth.blockNumber
      if BlockWithNumber bn > end
        then do
          lift $ Eth.uninstallFilter fid
          stop
        else do
          liftIO $ threadDelay 1000
          changes <- lift $ Eth.getFilterChanges fid
          yield $ mkFilterChanges changes
          pollPlan fid end

mkFilterChanges :: forall i ni e.
                   ( Event e
                   , DecodeEvent i ni e
                   )
                => [Change]
                -> [FilterChange e]
mkFilterChanges cs =
  flip mapMaybe cs $ \c@Change{..} -> do
    x <- decodeEvent c
    return $ FilterChange c x

data FilterStreamState = FilterStreamState { fssCurrentBlock  :: BlockNumber
                                           , fssInitialFilter :: Filter
                                           , fssWindowSize    :: Integer
                                           }

filterStream :: Provider p
             => FilterStreamState
             -> MachineT (Web3 p) k (Filter)
filterStream initialPlan = unfoldPlan initialPlan filterPlan
  where
    filterPlan :: Provider p => FilterStreamState -> PlanT k Filter (Web3 p) FilterStreamState
    filterPlan initialState@FilterStreamState{..} = do
      end <- lift . mkBlockNumber $ filterToBlock fssInitialFilter
      if fssCurrentBlock > end
        then stop
        else do
          let to' = newTo end fssCurrentBlock fssWindowSize
              filter' = fssInitialFilter { filterFromBlock = BlockWithNumber fssCurrentBlock
                                         , filterToBlock = BlockWithNumber to'
                                         }
          yield filter'
          filterPlan $ initialState { fssCurrentBlock = succ to' }
    succ :: BlockNumber -> BlockNumber
    succ (BlockNumber bn) = BlockNumber $ bn + 1
    newTo :: BlockNumber -> BlockNumber -> Integer -> BlockNumber
    newTo upper (BlockNumber current) window = min upper . BlockNumber $ current + window

mkBlockNumber :: Provider p => DefaultBlock -> Web3 p BlockNumber
mkBlockNumber bm = case bm of
  BlockWithNumber bn -> return bn
  Earliest -> return 0
  _ -> Eth.blockNumber

--------------------------------------------------------------------------------
-- * Transactions and Calls
--------------------------------------------------------------------------------

class Method a where
  selector :: Proxy a -> T.Text

-- | 'sendTx' is used to submit a state changing transaction.
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

-- | 'call' is used to call contract methods that have no state changing effects,
-- | or to call m
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
