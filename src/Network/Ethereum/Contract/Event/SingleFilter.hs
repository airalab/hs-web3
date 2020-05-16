{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

-- |
-- Module      :  Network.Ethereum.Contract.Event.SingleFilter
-- Copyright   :  FOAM team <http://foam.space> 2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- Contract event filters.
--

module Network.Ethereum.Contract.Event.SingleFilter
    (
      event
    , eventMany
    , eventNoFilter
    , eventManyNoFilter
    ) where

import           Control.Concurrent                     (threadDelay)
import           Control.Monad                          (forM, void, when)
import           Control.Monad.IO.Class                 (MonadIO (..))
import           Control.Monad.Trans.Class              (lift)
import           Control.Monad.Trans.Reader             (ReaderT (..))
import           Data.Machine                           (MachineT, asParts,
                                                         autoM, await,
                                                         construct, final,
                                                         repeatedly, runT,
                                                         unfoldPlan, (~>))
import           Data.Machine.Plan                      (PlanT, stop, yield)
import           Data.Maybe                             (catMaybes, listToMaybe)

import           Data.Solidity.Event                    (DecodeEvent (..))
import qualified Network.Ethereum.Api.Eth               as Eth
import           Network.Ethereum.Api.Types             (Change (..),
                                                         DefaultBlock (..),
                                                         Filter (..), Quantity)
import           Network.Ethereum.Contract.Event.Common
import           Network.JsonRpc.TinyClient             (JsonRpc (..))

-- | Run one block at a time.
event :: (DecodeEvent i ni e, JsonRpc m)
       => Filter e
       -> (e -> ReaderT Change m EventAction)
       -> m ()
event fltr = eventMany fltr 0

-- | 'eventMany' take s a filter, a window size, and a handler.
--
-- It runs the handler over the results of 'eventLogs' results using
-- 'reduceEventStream'. If no 'TerminateEvent' action is thrown and
-- the toBlock is not yet reached, it then transitions to polling.
--
eventMany :: (DecodeEvent i ni e, JsonRpc m)
           => Filter e
           -> Integer
           -> (e -> ReaderT Change m EventAction)
           -> m ()
eventMany fltr window handler = do
    start <- mkBlockNumber $ filterFromBlock fltr
    let initState = FilterStreamState { fssCurrentBlock = start
                                      , fssInitialFilter = fltr
                                      , fssWindowSize = window
                                      }
    mLastProcessedFilterState <- reduceEventStream (playOldLogs initState) handler
    case mLastProcessedFilterState of
      Nothing -> startPolling fltr {filterFromBlock = BlockWithNumber start}
      Just (act, lastBlock) -> do
        end <- mkBlockNumber . filterToBlock $ fltr
        when (act /= TerminateEvent && lastBlock < end) $
          let pollingFromBlock = lastBlock + 1
          in startPolling fltr {filterFromBlock = BlockWithNumber pollingFromBlock}
  where
    startPolling fltr' = do
      filterId <- Eth.newFilter fltr'
      let pollTo = filterToBlock fltr'
      void $ reduceEventStream (pollFilter filterId pollTo) handler

-- | Effectively a mapM_ over the machine using the given handler.
reduceEventStream :: Monad m
                  => MachineT m k [FilterChange a]
                  -> (a -> ReaderT Change m EventAction)
                  -> m (Maybe (EventAction, Quantity))
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
                   -> m [(EventAction, Quantity)]
    processChanges handler' changes = fmap catMaybes $
        forM changes $ \FilterChange{..} -> do
            act <- flip runReaderT filterChangeRawChange $
                handler' filterChangeEvent
            return ((,) act <$> changeBlockNumber filterChangeRawChange)

-- | 'playLogs' streams the 'filterStream' and calls eth_getLogs on these 'Filter' objects.
playOldLogs :: (DecodeEvent i ni e, JsonRpc m)
            => FilterStreamState e
            -> MachineT m k [FilterChange e]
playOldLogs s = filterStream s
          ~> autoM Eth.getLogs
          ~> autoM (liftIO . mkFilterChanges)

-- | Polls a filter from the given filterId until the target toBlock is reached.
pollFilter :: forall i ni e k m . (DecodeEvent i ni e, JsonRpc m)
           => Quantity
           -> DefaultBlock
           -> MachineT m k [FilterChange e]
pollFilter i = construct . pollPlan i
  where
    pollPlan :: Quantity -> DefaultBlock -> PlanT k [FilterChange e] m ()
    pollPlan fid end = do
      bn <- lift $ Eth.blockNumber
      if BlockWithNumber bn > end
        then do
          _ <- lift $ Eth.uninstallFilter fid
          stop
        else do
          liftIO $ threadDelay 1000000
          changes <- lift $ Eth.getFilterChanges fid >>= liftIO . mkFilterChanges
          yield $ changes
          pollPlan fid end


-- | 'filterStream' is a machine which represents taking an initial filter
-- over a range of blocks b1, ... bn (where bn is possibly `Latest` or `Pending`,
-- but b1 is an actual block number), and making a stream of filter objects
-- which cover this filter in intervals of size `windowSize`. The machine
-- halts whenever the `fromBlock` of a spanning filter either (1) excedes then
-- initial filter's `toBlock` or (2) is greater than the chain head's block number.
filterStream :: JsonRpc m
             => FilterStreamState e
             -> MachineT m k (Filter e)
filterStream initialPlan = unfoldPlan initialPlan filterPlan
  where
    filterPlan :: JsonRpc m => FilterStreamState e -> PlanT k (Filter e) m (FilterStreamState e)
    filterPlan initialState@FilterStreamState{..} = do
      end <- lift . mkBlockNumber $ filterToBlock fssInitialFilter
      if fssCurrentBlock > end
        then stop
        else do
          let to' = min end $ fssCurrentBlock + fromInteger fssWindowSize
              filter' = fssInitialFilter { filterFromBlock = BlockWithNumber fssCurrentBlock
                                         , filterToBlock = BlockWithNumber to'
                                         }
          yield filter'
          filterPlan $ initialState { fssCurrentBlock = to' + 1 }

--------------------------------------------------------------------------------

eventNoFilter :: (DecodeEvent i ni e, JsonRpc m)
              => Filter e
              -> (e -> ReaderT Change m EventAction)
              -> m ()
eventNoFilter fltr = eventManyNoFilter fltr 0

eventManyNoFilter :: (DecodeEvent i ni e, JsonRpc m)
                  => Filter e
                  -> Integer
                  -> (e -> ReaderT Change m EventAction)
                  -> m ()
eventManyNoFilter fltr window handler = do
    start <- mkBlockNumber $ filterFromBlock fltr
    let initState = FilterStreamState { fssCurrentBlock = start
                                      , fssInitialFilter = fltr
                                      , fssWindowSize = window
                                      }
    mLastProcessedFilterState <- reduceEventStream (playOldLogs initState) handler
    case mLastProcessedFilterState of
      Nothing ->
        let pollingFilterState = FilterStreamState { fssCurrentBlock = start
                                                   , fssInitialFilter = fltr
                                                   , fssWindowSize = 1
                                                   }

        in void $ reduceEventStream (playNewLogs pollingFilterState) handler
      Just (act, lastBlock) -> do
        end <- mkBlockNumber . filterToBlock $ fltr
        when (act /= TerminateEvent && lastBlock < end) $
          let pollingFilterState = FilterStreamState { fssCurrentBlock = lastBlock + 1
                                                     , fssInitialFilter = fltr
                                                     , fssWindowSize = 1
                                                     }
          in void $ reduceEventStream (playNewLogs pollingFilterState) handler

-- | 'playLogs' streams the 'filterStream' and calls eth_getLogs on these 'Filter' objects.
playNewLogs :: (DecodeEvent i ni e, JsonRpc m)
            => FilterStreamState e
            -> MachineT m k [FilterChange e]
playNewLogs s =
     newFilterStream s
  ~> autoM Eth.getLogs
  ~> autoM (liftIO . mkFilterChanges)

newFilterStream :: JsonRpc m
                => FilterStreamState e
                -> MachineT m k (Filter e)
newFilterStream initialState = unfoldPlan initialState filterPlan
  where
    filterPlan :: JsonRpc m => FilterStreamState e -> PlanT k (Filter e) m (FilterStreamState e)
    filterPlan s@FilterStreamState{..} = do
      if BlockWithNumber fssCurrentBlock > filterToBlock fssInitialFilter
        then stop
        else do
          newestBlockNumber <- lift . pollTillBlockProgress $ fssCurrentBlock
          let filter' = fssInitialFilter { filterFromBlock = BlockWithNumber fssCurrentBlock
                                         , filterToBlock = BlockWithNumber newestBlockNumber
                                         }
          yield filter'
          filterPlan $ s { fssCurrentBlock = newestBlockNumber + 1 }
