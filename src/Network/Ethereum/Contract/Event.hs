{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}


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

module Network.Ethereum.Contract.Event
  ( EventAction(..)

  -- * Single event monitors
  , event
  , event'
  , eventMany'

  -- * MultiEventMonitors
  , MultiFilter(..)
  , minBlock
  , modifyMultiFilter
  , multiEvent
  , multiEvent'
  , multiEventMany'

  -- * ReExports
  , Handlers
  , Handler(..)
  , Rec(..)

  ) where

import           Control.Concurrent             (threadDelay)
import           Control.Concurrent.Async       (Async)
import           Control.Exception              (Exception, throwIO)
import           Control.Monad                  (forM, void, when)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.Reader     (ReaderT (..))
import           Data.Either                    (lefts, rights)
import           Data.List                      (sortOn)
import           Data.Machine                   (MachineT, asParts, autoM,
                                                 await, construct, final,
                                                 repeatedly, runT, unfoldPlan,
                                                 (~>))
import           Data.Machine.Plan              (PlanT, stop, yield)
import           Data.Maybe                     (catMaybes, fromJust,
                                                 listToMaybe)
import           Data.Monoid                    ((<>))
import           Data.Proxy                     (Proxy (..))
import           Data.Tagged
import           Data.Vinyl
import           Data.Vinyl.CoRec
import           Data.Vinyl.Functor
import           Data.Vinyl.TypeLevel
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
                                   }

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
-- | MultiFilters
--------------------------------------------------------------------------------

data MultiFilter (es :: [*]) where
  NilFilters :: MultiFilter '[]
  (:?) :: Filter e -> MultiFilter es -> MultiFilter (e ': es)

infixr 5 :?

data MultiFilterStreamState es =
  MultiFilterStreamState { mfssCurrentBlock       :: Quantity
                         , mfssInitialMultiFilter :: MultiFilter es
                         , mfssWindowSize         :: Integer
                         }

multiFilterStream
  :: MultiFilterStreamState es
  -> MachineT Web3 k (MultiFilter es)
multiFilterStream initialPlan = do
  unfoldPlan initialPlan $ \s -> do
    end <- lift . mkBlockNumber . minBlock . mfssInitialMultiFilter $ initialPlan
    filterPlan end s
  where
    filterPlan :: Quantity -> MultiFilterStreamState es -> PlanT k (MultiFilter es) Web3 (MultiFilterStreamState es)
    filterPlan end initialState@MultiFilterStreamState{..} = do
      if mfssCurrentBlock > end
        then stop
        else do
          let to' = min end $ mfssCurrentBlock + fromInteger mfssWindowSize
              h :: forall e . Filter e -> Filter e
              h f = f { filterFromBlock = BlockWithNumber mfssCurrentBlock
                      , filterToBlock = BlockWithNumber to'
                      }
          yield (modifyMultiFilter h mfssInitialMultiFilter)
          filterPlan end initialState { mfssCurrentBlock = to' + 1 }

minBlock
  :: MultiFilter es
  -> DefaultBlock
minBlock NilFilters             = Pending
minBlock (Filter _ _ e _ :? fs) = e `min` minBlock fs

modifyMultiFilter
  :: (forall e. Filter e -> Filter e)
  -> MultiFilter es
  -> MultiFilter es
modifyMultiFilter _ NilFilters = NilFilters
modifyMultiFilter h (f :? fs)  = (h f :? modifyMultiFilter h fs)

weakenCoRec
  :: ( RecApplicative ts
     , FoldRec (t ': ts) (t ': ts)
     )
  => Field ts
  -> Field (t ': ts)
weakenCoRec = fromJust . firstField . (Compose Nothing :&) . coRecToRec

type family WithChange (es :: [*]) = (es' :: [*]) | es' -> es where
  WithChange '[] = '[]
  WithChange (e : es) = FilterChange e : WithChange es

class QueryAllLogs (es :: [*]) where
  queryAllLogs :: MultiFilter es -> Web3 [Field (WithChange es)]

instance QueryAllLogs '[] where
  queryAllLogs NilFilters = pure []

instance forall e i ni es.
  ( DecodeEvent i ni e
  , QueryAllLogs es
  , RecApplicative (WithChange es)
  , FoldRec (FilterChange e : WithChange es) (WithChange es)
  ) => QueryAllLogs (e:es) where

  queryAllLogs (f  :? fs) = do
    changes <- Eth.getLogs f
    filterChanges <- liftIO . mkFilterChanges @_ @_ @e $ changes
    filterChanges' <- queryAllLogs fs
    pure $ map (CoRec . Identity) filterChanges <> map weakenCoRec filterChanges'

class HasLogIndex a where
  getLogIndex :: a -> Maybe (Quantity, Quantity)

instance HasLogIndex (FilterChange e) where
  getLogIndex FilterChange{..} =
    (,) <$> changeBlockNumber filterChangeRawChange <*> changeLogIndex filterChangeRawChange

sortChanges
  :: ( AllAllSat '[HasLogIndex] es
     , RecApplicative es
     )
  => [Field es]
  -> [Field es]
sortChanges changes =
  let sorterProj change = onField (Proxy @'[HasLogIndex]) getLogIndex change
  in sortOn sorterProj changes

class MapHandlers m es es' where
  mapHandlers
    :: Handlers es (ReaderT Change m EventAction)
    -> Handlers es' (m (Maybe (EventAction, Quantity)))

instance Monad m => MapHandlers m '[] '[] where
  mapHandlers RNil = RNil

instance
  ( Monad m
  , MapHandlers m es es'
  ) => MapHandlers m (e : es) (FilterChange e : es') where

  mapHandlers ((H f) :& fs) =
    let f' = \FilterChange{..} -> do
          act <- runReaderT (f filterChangeEvent) filterChangeRawChange
          return ((,) act <$> changeBlockNumber filterChangeRawChange)
    in (H f') :& mapHandlers fs


reduceMultiEventStream
  :: ( Monad m
     , MapHandlers m es (WithChange es)
     )
  => MachineT m k [Field (WithChange es)]
  -> Handlers es (ReaderT Change m EventAction)
  -> m (Maybe (EventAction, Quantity))
reduceMultiEventStream filterChanges handlers = fmap listToMaybe . runT $
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

-- | 'playLogs' streams the 'filterStream' and calls eth_getLogs on these 'Filter' objects.
playMultiLogs
  :: forall es k.
     ( QueryAllLogs es
     , AllAllSat '[HasLogIndex] (WithChange es)
     , RecApplicative (WithChange es)
     )
  => MultiFilterStreamState es
  -> MachineT Web3 k [Field (WithChange es)]
playMultiLogs s = fmap sortChanges $
     multiFilterStream s
  ~> autoM queryAllLogs

data TaggedFilterIds (es :: [*]) where
  TaggedFilterNil :: TaggedFilterIds '[]
  TaggedFilterCons :: Tagged e Quantity -> TaggedFilterIds es -> TaggedFilterIds (e : es)

class PollFilters (es :: [*]) where
  openMultiFilter :: MultiFilter es -> Web3 (TaggedFilterIds es)
  checkMultiFilter :: TaggedFilterIds es -> Web3 [Field (WithChange es)]
  closeMultiFilter :: TaggedFilterIds es -> Web3 ()

instance PollFilters '[] where
  openMultiFilter _ = pure TaggedFilterNil
  checkMultiFilter _ = pure []
  closeMultiFilter _ = pure ()

instance forall e i ni es.
  ( DecodeEvent i ni e
  , PollFilters es
  , RecApplicative (WithChange es)
  , FoldRec (FilterChange e : WithChange es) (WithChange es)
  ) => PollFilters (e:es) where
  openMultiFilter (f :? fs) = do
    fId <- Eth.newFilter f
    fsIds <- openMultiFilter fs
    pure $ TaggedFilterCons (Tagged fId) fsIds

  checkMultiFilter (TaggedFilterCons (Tagged fId) fsIds) = do
    changes <- Eth.getFilterChanges fId
    filterChanges <- liftIO . mkFilterChanges @_ @_ @e $ changes
    filterChanges' <- checkMultiFilter @es fsIds
    pure  $ map (CoRec . Identity) filterChanges <>  map weakenCoRec filterChanges'

  closeMultiFilter (TaggedFilterCons (Tagged fId) fsIds) = do
    _ <- Eth.uninstallFilter fId
    closeMultiFilter fsIds


-- | Polls a filter from the given filterId until the target toBlock is reached.
pollMultiFilter
  :: ( PollFilters es
     , RecApplicative (WithChange es)
     , AllAllSat '[HasLogIndex] (WithChange es)
     )
  => TaggedFilterIds es
  -> DefaultBlock
  -> MachineT Web3 k [Field (WithChange es)]
pollMultiFilter is = construct . pollPlan is
  where
    -- pollPlan :: TaggedFilterIds es -> DefaultBlock -> PlanT k [Field (Map (TyCon1 FilterChange) es)] Web3 ()
    pollPlan (fIds :: TaggedFilterIds es) end = do
      bn <- lift $ Eth.blockNumber
      if BlockWithNumber bn > end
        then do
          _ <- lift $ closeMultiFilter fIds
          stop
        else do
          liftIO $ threadDelay 1000000
          changes <- lift $ sortChanges <$> checkMultiFilter fIds
          yield changes
          pollPlan fIds end

multiEventMany'
  :: ( PollFilters es
     , QueryAllLogs es
     , MapHandlers Web3 es (WithChange es)
     , AllAllSat '[HasLogIndex] (WithChange es)
     , RecApplicative (WithChange es)
     )
  => MultiFilter es
  -> Integer
  -> Handlers es (ReaderT Change Web3 EventAction)
  -> Web3 ()
multiEventMany' fltrs window handlers = do
    start <- mkBlockNumber $ minBlock fltrs
    let initState =
          MultiFilterStreamState { mfssCurrentBlock = start
                                 , mfssInitialMultiFilter = fltrs
                                 , mfssWindowSize = window
                                 }
    mLastProcessedFilterState <- reduceMultiEventStream (playMultiLogs initState) handlers
    case mLastProcessedFilterState of
      Nothing -> startPolling (modifyMultiFilter (\f -> f {filterFromBlock = BlockWithNumber start}) fltrs)
      Just (act, lastBlock) -> do
        end <- mkBlockNumber . minBlock $ fltrs
        when (act /= TerminateEvent && lastBlock < end) $
          let pollingFromBlock = lastBlock + 1
          in startPolling (modifyMultiFilter (\f -> f {filterFromBlock = BlockWithNumber pollingFromBlock}) fltrs)
  where
    startPolling fltrs' = do
      fIds <- openMultiFilter fltrs'
      let pollTo = minBlock fltrs'
      void $ reduceMultiEventStream (pollMultiFilter fIds pollTo) handlers

multiEvent
  :: ( PollFilters es
     , QueryAllLogs es
     , MapHandlers Web3 es (WithChange es)
     , AllAllSat '[HasLogIndex] (WithChange es)
     , RecApplicative (WithChange es)
     )
  => MultiFilter es
  -> Handlers es (ReaderT Change Web3 EventAction)
  -> Web3 (Async ())
multiEvent fltrs = forkWeb3 . multiEvent' fltrs

multiEvent'
  :: ( PollFilters es
     , QueryAllLogs es
     , MapHandlers Web3 es (WithChange es)
     , AllAllSat '[HasLogIndex] (WithChange es)
     , RecApplicative (WithChange es)
     )
  => MultiFilter es
  -> Handlers es (ReaderT Change Web3 EventAction)
  -> Web3 ()
multiEvent' fltrs = multiEventMany' fltrs 0
