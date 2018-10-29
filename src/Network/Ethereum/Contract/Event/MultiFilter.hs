{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
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
-- Module      :  Network.Ethereum.Contract.Event.MultiFilter
-- Copyright   :  FOAM team <http://foam.space> 2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- Support for parallel multiple event filters.
--

module  Network.Ethereum.Contract.Event.MultiFilter
    (
    -- * The @MultiFilter@ type
      MultiFilter(..)
    , minStartBlock
    , minEndBlock
    , modifyMultiFilter

    -- * With geth filters
    , multiEvent
    , multiEvent'
    , multiEventMany'

    -- * Without geth filters
    , multiEventNoFilter
    , multiEventNoFilter'
    , multiEventManyNoFilter'

    -- * Re-exports
    , Handlers
    , Handler(..)
    , Rec(..)
    ) where

import           Control.Concurrent                     (threadDelay)
import           Control.Concurrent.Async               (Async)
import           Control.Monad                          (forM, void, when)
import           Control.Monad.IO.Class                 (MonadIO (..))
import           Control.Monad.Trans.Class              (lift)
import           Control.Monad.Trans.Reader             (ReaderT (..))
import           Data.List                              (sortOn)
import           Data.Machine                           (MachineT, asParts,
                                                         autoM, await,
                                                         construct, final,
                                                         repeatedly, runT,
                                                         unfoldPlan, (~>))
import           Data.Machine.Plan                      (PlanT, stop, yield)
import           Data.Maybe                             (catMaybes, fromJust,
                                                         listToMaybe)
import           Data.Monoid                            ((<>))
import           Data.Tagged                            (Tagged (..))
import           Data.Vinyl                             (Rec ((:&), RNil),
                                                         RecApplicative)
import           Data.Vinyl.CoRec                       (CoRec (..), Field,
                                                         FoldRec, Handler (H),
                                                         Handlers, coRecToRec,
                                                         firstField, match,
                                                         onField)
import           Data.Vinyl.Functor                     (Compose (..),
                                                         Identity (..))
#if MIN_VERSION_vinyl(0,10,0)
import           Data.Vinyl                             (RPureConstrained)
#else
import           Data.Proxy                             (Proxy (..))
import           Data.Vinyl.TypeLevel                   (AllAllSat)
#endif

import           Data.Solidity.Event                    (DecodeEvent (..))
import qualified Network.Ethereum.Api.Eth               as Eth
import           Network.Ethereum.Api.Provider          (Web3, forkWeb3)
import           Network.Ethereum.Api.Types             (Change (..),
                                                         DefaultBlock (..),
                                                         Filter (..), Quantity)
import           Network.Ethereum.Contract.Event.Common

--------------------------------------------------------------------------------
-- | MultiFilters
--------------------------------------------------------------------------------

data MultiFilter (es :: [*]) where
  NilFilters :: MultiFilter '[]
  (:?) :: Filter e -> MultiFilter es -> MultiFilter (e ': es)

infixr 5 :?

minEndBlock
  :: MultiFilter es
  -> DefaultBlock
minEndBlock NilFilters             = Pending
minEndBlock (Filter _ _ e _ :? fs) = e `min` minEndBlock fs

minStartBlock
  :: MultiFilter es
  -> DefaultBlock
minStartBlock NilFilters             = Pending
minStartBlock (Filter _ s _ _ :? fs) = s `min` minStartBlock fs

modifyMultiFilter
  :: (forall e. Filter e -> Filter e)
  -> MultiFilter es
  -> MultiFilter es
modifyMultiFilter _ NilFilters = NilFilters
modifyMultiFilter h (f :? fs)  = h f :? modifyMultiFilter h fs


multiEvent
  :: ( PollFilters es
     , QueryAllLogs es
     , MapHandlers Web3 es (WithChange es)
#if MIN_VERSION_vinyl(0,10,0)
     , RPureConstrained HasLogIndex (WithChange es)
#else
     , AllAllSat '[HasLogIndex] (WithChange es)
#endif
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
#if MIN_VERSION_vinyl(0,10,0)
     , RPureConstrained HasLogIndex (WithChange es)
#else
     , AllAllSat '[HasLogIndex] (WithChange es)
#endif
     , RecApplicative (WithChange es)
     )
  => MultiFilter es
  -> Handlers es (ReaderT Change Web3 EventAction)
  -> Web3 ()
multiEvent' fltrs = multiEventMany' fltrs 0

data MultiFilterStreamState es =
  MultiFilterStreamState { mfssCurrentBlock       :: Quantity
                         , mfssInitialMultiFilter :: MultiFilter es
                         , mfssWindowSize         :: Integer
                         }


multiEventMany'
  :: ( PollFilters es
     , QueryAllLogs es
     , MapHandlers Web3 es (WithChange es)
#if MIN_VERSION_vinyl(0,10,0)
     , RPureConstrained HasLogIndex (WithChange es)
#else
     , AllAllSat '[HasLogIndex] (WithChange es)
#endif
     , RecApplicative (WithChange es)
     )
  => MultiFilter es
  -> Integer
  -> Handlers es (ReaderT Change Web3 EventAction)
  -> Web3 ()
multiEventMany' fltrs window handlers = do
    start <- mkBlockNumber $ minStartBlock fltrs
    let initState =
          MultiFilterStreamState { mfssCurrentBlock = start
                                 , mfssInitialMultiFilter = fltrs
                                 , mfssWindowSize = window
                                 }
    mLastProcessedFilterState <- reduceMultiEventStream (playMultiLogs initState) handlers
    case mLastProcessedFilterState of
      Nothing -> startPolling (modifyMultiFilter (\f -> f {filterFromBlock = BlockWithNumber start}) fltrs)
      Just (act, lastBlock) -> do
        end <- mkBlockNumber . minEndBlock $ fltrs
        when (act /= TerminateEvent && lastBlock < end) $
          let pollingFromBlock = lastBlock + 1
          in startPolling (modifyMultiFilter (\f -> f {filterFromBlock = BlockWithNumber pollingFromBlock}) fltrs)
  where
    startPolling fltrs' = do
      fIds <- openMultiFilter fltrs'
      let pollTo = minEndBlock fltrs'
      void $ reduceMultiEventStream (pollMultiFilter fIds pollTo) handlers

multiFilterStream
  :: MultiFilterStreamState es
  -> MachineT Web3 k (MultiFilter es)
multiFilterStream initialPlan = do
  unfoldPlan initialPlan $ \s -> do
    end <- lift . mkBlockNumber . minEndBlock . mfssInitialMultiFilter $ initialPlan
    filterPlan end s
  where
    filterPlan :: Quantity -> MultiFilterStreamState es -> PlanT k (MultiFilter es) Web3 (MultiFilterStreamState es)
    filterPlan end initialState@MultiFilterStreamState{..} = do
      if mfssCurrentBlock > end
        then stop
        else do
          let to' = min end $ mfssCurrentBlock + fromInteger mfssWindowSize
              h :: forall e. Filter e -> Filter e
              h f = f { filterFromBlock = BlockWithNumber mfssCurrentBlock
                      , filterToBlock = BlockWithNumber to'
                      }
          yield (modifyMultiFilter h mfssInitialMultiFilter)
          filterPlan end initialState { mfssCurrentBlock = to' + 1 }

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
#if MIN_VERSION_vinyl(0,10,0)
  :: ( RPureConstrained HasLogIndex es
#else
  :: ( AllAllSat '[HasLogIndex] es
#endif
     , RecApplicative es
     )
  => [Field es]
  -> [Field es]
sortChanges changes =
#if MIN_VERSION_vinyl(0,10,0)
  let sorterProj change = onField @HasLogIndex getLogIndex change
#else
  let sorterProj change = onField (Proxy @'[HasLogIndex]) getLogIndex change
#endif
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

  mapHandlers (H f :& fs) =
    let f' FilterChange{..} = do
          act <- runReaderT (f filterChangeEvent) filterChangeRawChange
          return ((,) act <$> changeBlockNumber filterChangeRawChange)
    in H f' :& mapHandlers fs


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
#if MIN_VERSION_vinyl(0,10,0)
     , RPureConstrained HasLogIndex (WithChange es)
#else
     , AllAllSat '[HasLogIndex] (WithChange es)
#endif
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
#if MIN_VERSION_vinyl(0,10,0)
     , RPureConstrained HasLogIndex (WithChange es)
#else
     , AllAllSat '[HasLogIndex] (WithChange es)
#endif
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

--------------------------------------------------------------------------------


multiEventNoFilter
  :: ( QueryAllLogs es
     , MapHandlers Web3 es (WithChange es)
#if MIN_VERSION_vinyl(0,10,0)
     , RPureConstrained HasLogIndex (WithChange es)
#else
     , AllAllSat '[HasLogIndex] (WithChange es)
#endif
     , RecApplicative (WithChange es)
     )
  => MultiFilter es
  -> Handlers es (ReaderT Change Web3 EventAction)
  -> Web3 (Async ())
multiEventNoFilter fltrs = forkWeb3 . multiEventNoFilter' fltrs

multiEventNoFilter'
  :: ( QueryAllLogs es
     , MapHandlers Web3 es (WithChange es)
#if MIN_VERSION_vinyl(0,10,0)
     , RPureConstrained HasLogIndex (WithChange es)
#else
     , AllAllSat '[HasLogIndex] (WithChange es)
#endif
     , RecApplicative (WithChange es)
     )
  => MultiFilter es
  -> Handlers es (ReaderT Change Web3 EventAction)
  -> Web3 ()
multiEventNoFilter' fltrs = multiEventManyNoFilter' fltrs 0


multiEventManyNoFilter'
  :: ( QueryAllLogs es
     , MapHandlers Web3 es (WithChange es)
#if MIN_VERSION_vinyl(0,10,0)
     , RPureConstrained HasLogIndex (WithChange es)
#else
     , AllAllSat '[HasLogIndex] (WithChange es)
#endif
     , RecApplicative (WithChange es)
     )
  => MultiFilter es
  -> Integer
  -> Handlers es (ReaderT Change Web3 EventAction)
  -> Web3 ()
multiEventManyNoFilter' fltrs window handlers = do
    start <- mkBlockNumber $ minStartBlock fltrs
    let initState =
          MultiFilterStreamState { mfssCurrentBlock = start
                                 , mfssInitialMultiFilter = fltrs
                                 , mfssWindowSize = window
                                 }
    mLastProcessedFilterState <- reduceMultiEventStream (playMultiLogs initState) handlers
    case mLastProcessedFilterState of
      Nothing ->
        let pollingFilterState =
              MultiFilterStreamState { mfssCurrentBlock = start
                                     , mfssInitialMultiFilter = fltrs
                                     , mfssWindowSize = 0
                                     }
        in void $ reduceMultiEventStream (playNewMultiLogs pollingFilterState) handlers
      Just (act, lastBlock) -> do
        end <- mkBlockNumber . minEndBlock $ fltrs
        when (act /= TerminateEvent && lastBlock < end) $
          let pollingFilterState = MultiFilterStreamState { mfssCurrentBlock = lastBlock + 1
                                                          , mfssInitialMultiFilter = fltrs
                                                          , mfssWindowSize = 0
                                                          }
          in void $ reduceMultiEventStream (playNewMultiLogs pollingFilterState) handlers

newMultiFilterStream
  :: MultiFilterStreamState es
  -> MachineT Web3 k (MultiFilter es)
newMultiFilterStream initialPlan = do
  unfoldPlan initialPlan $ \s -> do
    let end = minEndBlock . mfssInitialMultiFilter $ initialPlan
    filterPlan end s
  where
    filterPlan :: DefaultBlock -> MultiFilterStreamState es -> PlanT k (MultiFilter es) Web3 (MultiFilterStreamState es)
    filterPlan end initialState@MultiFilterStreamState{..} = do
      if BlockWithNumber mfssCurrentBlock > end
        then stop
        else do
          newestBlockNumber <- lift . pollTillBlockProgress $ mfssCurrentBlock
          let h :: forall e. Filter e -> Filter e
              h f = f { filterFromBlock = BlockWithNumber mfssCurrentBlock
                      , filterToBlock = BlockWithNumber newestBlockNumber
                      }
          yield (modifyMultiFilter h mfssInitialMultiFilter)
          filterPlan end initialState { mfssCurrentBlock = newestBlockNumber + 1 }

playNewMultiLogs
  :: forall es k.
     ( QueryAllLogs es
#if MIN_VERSION_vinyl(0,10,0)
     , RPureConstrained HasLogIndex (WithChange es)
#else
     , AllAllSat '[HasLogIndex] (WithChange es)
#endif
     , RecApplicative (WithChange es)
     )
  => MultiFilterStreamState es
  -> MachineT Web3 k [Field (WithChange es)]
playNewMultiLogs s = fmap sortChanges $
     newMultiFilterStream s
  ~> autoM queryAllLogs
