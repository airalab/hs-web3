{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}


-- |
-- Module      :  Network.Ethereum.Contract.Event
-- Copyright   :  Alexander Krupenkin 2016-2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- Ethereum contract event support.
--

module Network.Ethereum.Contract.Event where

import           Control.Concurrent             (threadDelay)
import           Control.Exception              (Exception, throwIO)
import           Control.Monad                  (forM, void, when)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.Reader     (ReaderT (..))
import           Data.Type.List
import           Data.Either                    (rights, lefts)
import           Data.Machine                   (MachineT, asParts, autoM,
                                                 await, construct, final,
                                                 repeatedly, runT,
                                                 unfoldPlan, (~>))
import Data.Vinyl.Functor (Identity(..))
import Data.List (sortOn)
import Unsafe.Coerce (unsafeCoerce)
import Data.Singletons (TyCon1)
import Data.Monoid ((<>))
import           Data.Machine.Plan              (PlanT, stop, yield)
import           Data.Maybe                     (catMaybes, listToMaybe)
import Data.Proxy (Proxy(..))
import           Data.Vinyl.CoRec
import           Data.Vinyl
import           Control.Concurrent.Async       (Async)
import           Network.Ethereum.ABI.Event     (DecodeEvent (..))
import qualified Network.Ethereum.Web3.Eth      as Eth
import           Network.Ethereum.Web3.Provider (Web3, forkWeb3)
import           Network.Ethereum.Web3.Types    (Change (..), DefaultBlock (..),
                                                 Filter (..), Quantity)

-- | Event callback control response
data EventAction = ContinueEvent
                 -- ^ Continue to listen events
                 | TerminateEvent
                 -- ^ Terminate event listener
  deriving (Show, Eq)

-- | Run 'event\'' one block at a time.
event :: DecodeEvent i ni e
      => Filter e
      -> (e -> ReaderT Change Web3 EventAction)
      -> Web3 (Async ())
event fltr = forkWeb3 . event' fltr

-- | Same as 'event', but does not immediately spawn a new thread.
event' :: DecodeEvent i ni e
       => Filter e
       -> (e -> ReaderT Change Web3 EventAction)
       -> Web3 ()
event' fltr = eventMany' fltr 0

-- | 'eventMany\'' take s a filter, a window size, and a handler.
--
-- It runs the handler over the results of 'eventLogs' results using
-- 'reduceEventStream'. If no 'TerminateEvent' action is thrown and
-- the toBlock is not yet reached, it then transitions to polling.
--
eventMany' :: DecodeEvent i ni e
           => Filter e
           -> Integer
           -> (e -> ReaderT Change Web3 EventAction)
           -> Web3 ()
eventMany' fltr window handler = do
    start <- mkBlockNumber $ filterFromBlock fltr
    let initState = FilterStreamState { fssCurrentBlock = start
                                      , fssInitialFilter = fltr
                                      , fssWindowSize = window
                                      }
    mLastProcessedFilterState <- reduceEventStream (playLogs initState) handler
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

data FilterChange a = FilterChange { filterChangeRawChange :: Change
                                   , filterChangeEvent     :: a
                                   } deriving Functor

-- | 'playLogs' streams the 'filterStream' and calls eth_getLogs on these 'Filter' objects.
playLogs :: DecodeEvent i ni e
         => FilterStreamState e
         -> MachineT Web3 k [FilterChange e]
playLogs s = filterStream s
          ~> autoM Eth.getLogs
          ~> autoM (liftIO . mkFilterChanges)

-- | Polls a filter from the given filterId until the target toBlock is reached.
pollFilter :: forall i ni e k . DecodeEvent i ni e
           => Quantity
           -> DefaultBlock
           -> MachineT Web3 k [FilterChange e]
pollFilter i = construct . pollPlan i
  where
    pollPlan :: Quantity -> DefaultBlock -> PlanT k [FilterChange e] Web3 ()
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

data EventParseFailure = EventParseFailure String deriving (Show)

instance Exception EventParseFailure

mkFilterChanges :: DecodeEvent i ni e
                => [Change]
                -> IO [FilterChange e]
mkFilterChanges changes =
  let eChanges = map (\c@Change{..} -> FilterChange c <$> decodeEvent c) changes
      ls = lefts eChanges
      rs = rights eChanges
  in if ls /= [] then throwIO (EventParseFailure $ (show ls)) else pure rs

data FilterStreamState e =
  FilterStreamState { fssCurrentBlock  :: Quantity
                    , fssInitialFilter :: Filter e
                    , fssWindowSize    :: Integer
                    }


-- | 'filterStream' is a machine which represents taking an initial filter
-- over a range of blocks b1, ... bn (where bn is possibly `Latest` or `Pending`,
-- but b1 is an actual block number), and making a stream of filter objects
-- which cover this filter in intervals of size `windowSize`. The machine
-- halts whenever the `fromBlock` of a spanning filter either (1) excedes then
-- initial filter's `toBlock` or (2) is greater than the chain head's block number.
filterStream :: FilterStreamState e
             -> MachineT Web3 k (Filter e)
filterStream initialPlan = unfoldPlan initialPlan filterPlan
  where
    filterPlan :: FilterStreamState e -> PlanT k (Filter e) Web3 (FilterStreamState e)
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

-- | Coerce a 'DefaultBlock' into a numerical block number.
mkBlockNumber :: DefaultBlock -> Web3 Quantity
mkBlockNumber bm = case bm of
  BlockWithNumber bn -> return bn
  Earliest           -> return 0
  _                  -> Eth.blockNumber

--------------------------------------------------------------------------------


data Filters (es :: [*]) where
  NilFilters :: Filters '[]
  (:?) :: Filter e -> Filters es -> Filters (e ': es)

data FiltersStreamState es =
  FiltersStreamState { fsssCurrentBlock   :: Quantity
                     , fsssInitialFilters :: Filters es
                     , fsssWindowSize     :: Integer
                     }

filtersStream :: FiltersStreamState es
              -> MachineT Web3 k (Filters es)
filtersStream initialPlan = do
  unfoldPlan initialPlan $ \s -> do
    end <- lift . mkBlockNumber . minEndBlock . fsssInitialFilters $ initialPlan
    filterPlan end s
  where
    filterPlan :: Quantity -> FiltersStreamState es -> PlanT k (Filters es) Web3 (FiltersStreamState es)
    filterPlan end initialState@FiltersStreamState{..} = do
      if fsssCurrentBlock > end
        then stop
        else do
          let to' = min end $ fsssCurrentBlock + fromInteger fsssWindowSize
              h :: forall e . Filter e -> Filter e
              h f = f { filterFromBlock = BlockWithNumber fsssCurrentBlock
                      , filterToBlock = BlockWithNumber to'
                      }
          yield (modifyFilters h fsssInitialFilters)
          filterPlan end initialState { fsssCurrentBlock = to' + 1 }
    minEndBlock :: Filters es -> DefaultBlock
    minEndBlock NilFilters = Pending
    minEndBlock (Filter _ _ e _ :? fs) = e `min` minEndBlock fs
    modifyFilters :: (forall e. Filter e -> Filter e) -> Filters es -> Filters es
    modifyFilters _ NilFilters = NilFilters
    modifyFilters h (f :? fs) = (h f :? modifyFilters h fs)

class QueryAllLogs (es :: [*]) where
  queryAllLogs :: Filters es -> Web3 [Field (Map (TyCon1 FilterChange) es)]

instance QueryAllLogs '[] where
  queryAllLogs NilFilters = pure []

-- The unsafeCoerce should actually be safe, it just needs to preserve
-- `RElem a es (RIndex a es) => RElem a (e:es) (RIndex a (e:es))` for any a in es. But I think
-- this is true since the type list is updated in both arguments of RElem in the same way.
instance forall e i ni es. (DecodeEvent i ni e, QueryAllLogs es) => QueryAllLogs (e:es) where
  queryAllLogs ((f  :: Filter e) :? (fs :: Filters es)) = do
    changes <- Eth.getLogs f
    filterChanges :: [FilterChange e] <- liftIO . mkFilterChanges $ changes
    filterChanges' :: [Field (Map (TyCon1 FilterChange) es)] <- queryAllLogs fs
    pure $ map (CoRec . Identity) filterChanges <> unsafeCoerce filterChanges'

-- Is unsafeCoerce actually safe here? If you can't resolve every variant to (Map (TyCon1 FilterChange))
-- then doesn't typecheck.
sortChanges
  :: Proxy es
  -> [Field (Map (TyCon1 FilterChange) es)]
  -> [Field (Map (TyCon1 FilterChange) es)]
sortChanges _ changes =
  let sorterProj :: forall e. FilterChange e -> Maybe (Quantity, Quantity)
      sorterProj = \FilterChange{..} ->
        (,) <$> changeBlockNumber filterChangeRawChange <*> changeLogIndex filterChangeRawChange
  in sortOn (\(CoRec (Identity fc)) -> sorterProj $ unsafeCoerce fc) changes

-- | Effectively a mapM_ over the machine using the given handler.
reduceEventStream'
  :: (Monad m, MapHandlers m es (Map (TyCon1 FilterChange) es))
  => MachineT m k [Field (Map (TyCon1 FilterChange) es)]
  -> Handlers es (ReaderT Change m EventAction)
  -> m (Maybe (EventAction, Quantity))
reduceEventStream' filterChanges handlers = fmap listToMaybe . runT $
       filterChanges
    ~> autoM (processChanges handlers)
    ~> asParts
    ~> runWhile (\(act, _) -> act /= TerminateEvent)
    ~> final
  where
    runWhile p = repeatedly $ do
      v <- await
      if p v
        then yield v
        else yield v >> stop
    processChanges handlers' changes = fmap catMaybes $
        forM changes $ \fc -> match fc (mapHandlers handlers')

class MapHandlers m es es' where
  mapHandlers
    :: Handlers es (ReaderT Change m EventAction)
    -> Handlers es' (m (Maybe (EventAction, Quantity)))

instance Monad m => MapHandlers m '[] '[] where
  mapHandlers RNil = RNil

instance (Monad m, MapHandlers m es es', es' ~ Map (TyCon1 FilterChange) es) => MapHandlers m (e : es) (FilterChange e : es') where
  mapHandlers ((H f) :& fs) =
    let f' = \FilterChange{..} -> do
          act <- runReaderT (f filterChangeEvent) filterChangeRawChange
          return ((,) act <$> changeBlockNumber filterChangeRawChange)
    in (H f') :& mapHandlers fs

-- | 'playLogs' streams the 'filterStream' and calls eth_getLogs on these 'Filter' objects.
playLogs' :: forall es k.
            QueryAllLogs es
         => FiltersStreamState es
         -> MachineT Web3 k [Field (Map (TyCon1 FilterChange) es)]
playLogs' s = filtersStream s
          ~> autoM queryAllLogs
          ~> autoM (return . sortChanges (Proxy @es))
