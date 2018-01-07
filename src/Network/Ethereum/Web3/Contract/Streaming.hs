{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Ethereum.Web3.Contract.Streaming where

import           Network.Ethereum.Unit
import           Network.Ethereum.Web3.Address
import           Network.Ethereum.Web3.Contract
import           Network.Ethereum.Web3.Encoding
import qualified Network.Ethereum.Web3.Eth      as Eth
import           Network.Ethereum.Web3.Provider
import           Network.Ethereum.Web3.Types

import           Control.Concurrent             (threadDelay)
import           Control.Monad                  (forM, void)
import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.Maybe      (MaybeT (..))
import           Control.Monad.Trans.Reader     (ReaderT (..), runReaderT)
import           Data.Machine                   (Is (Refl), ProcessT,
                                                 Step (Await, Stop, Yield),
                                                 autoT, runMachineT)
import           Data.Machine.MealyT            (MealyT (..), arrM)
import           Data.Maybe                     (mapMaybe)

data FilterChange a = FilterChange { filterChangeRawChange :: Change
                                   , filterChangeEvent     :: a
                                   }

-- the halting case is provided by Nothing
type HaltingMealyT m = MealyT (MaybeT m)

reduceEventStream :: forall m s a . Monad m
                  => HaltingMealyT m s [FilterChange a]
                  -> (a -> ReaderT Change m EventAction)
                  -> s
                  -> m (Maybe s)
reduceEventStream machine handler initialState =
  let process = autoT machine
  in go process

  where go :: ProcessT (MaybeT m) s [FilterChange a] -> m (Maybe s)
        go process = do
          res <- runMaybeT $ runMachineT process
          case res of
            Nothing   -> return $ Just initialState
            Just Stop -> return $ Just initialState -- Pretty sure this will never get called,
                                                    -- due to autoT for MealyT
            Just (Yield changes next) -> do
              acts <- processChanges handler changes
              if TerminateEvent `notElem` acts
              then go next
              else return Nothing
            Just (Await f Refl next) -> go (f initialState) >> go next -- This is a guess


        processChanges :: (a -> ReaderT Change m EventAction) -> [FilterChange a] -> m [EventAction]
        processChanges handler changes = forM changes $ \FilterChange{..} ->
                                           flip runReaderT filterChangeRawChange $
                                                handler filterChangeEvent

data FilterStreamState = FilterStreamState { fssCurrentBlock  :: BlockNumber
                                           , fssInitialFilter :: Filter
                                           , fssWindowSize    :: Integer
                                           }


-- | 'playLogs' streams the 'filterStream' and calls eth_getLogs on these
-- | 'Filter' objects.
playLogs :: forall p a. (Provider p, ABIDecode a)
         => HaltingMealyT (Web3 p) FilterStreamState [FilterChange a]
playLogs  = do
  filter <- filterStream
  changes <- arrM . const . lift $ Eth.getLogs filter
  return $ mkFilterChanges changes

pollFilter :: forall p a s . (Provider p, ABIDecode a)
           => FilterId
           -> DefaultBlock
           -> HaltingMealyT (Web3 p) s [FilterChange a]
pollFilter fid stop = MealyT $ \s -> do
  bn <- lift $ Eth.blockNumber
  if BlockWithNumber bn > stop
  then do
    lift $ Eth.uninstallFilter fid
    MaybeT $ return Nothing
  else do
    lift . Web3 $ threadDelay 1000
    changes <- lift $ Eth.getFilterChanges fid
    return (mkFilterChanges changes, pollFilter fid stop)

mkFilterChanges :: ABIDecode a => [Change] -> [FilterChange a]
mkFilterChanges cs = flip mapMaybe cs $ \c@Change{..} -> do
                       x <- fromData changeData
                       return $ FilterChange c x

filterStream :: forall p . Provider p
             => HaltingMealyT (Web3 p) FilterStreamState Filter
filterStream = MealyT $ \FilterStreamState{..} -> do
  end <- lift . mkBlockNumber $ filterToBlock fssInitialFilter
  if fssCurrentBlock > end
  then MaybeT $ return Nothing -- halt
  else return $ let to' = newTo end fssCurrentBlock fssWindowSize
                    filter' = fssInitialFilter { filterFromBlock = Just fssCurrentBlock
                                               , filterToBlock = Just to'
                                               }
                    MealyT next = filterStream
                in (filter', MealyT $ \s -> next s { fssCurrentBlock = succ to' })

      where succ :: BlockNumber -> BlockNumber
            succ (BlockNumber bn) = BlockNumber $ bn + 1

            newTo :: BlockNumber -> BlockNumber -> Integer -> BlockNumber
            newTo upper (BlockNumber current) window = min upper . BlockNumber $ current + window


event' :: (ABIDecode a, Provider p)
       => Filter
       -> Integer
       -> (a -> ReaderT Change (Web3 p) EventAction)
       -> Web3 p ()
event' fltr window handler = do
  start <- mkBlockNumber $ filterFromBlock fltr
  let initState = FilterStreamState { fssCurrentBlock = start
                                    , fssInitialFilter = fltr
                                    , fssWindowSize = window
                                    }
  mLastProcessedFilterState <- reduceEventStream playLogs handler initState
  case mLastProcessedFilterState of
    Nothing -> return ()
    Just lastProcessedFilterState -> do
      let BlockNumber lastBlock = fssCurrentBlock lastProcessedFilterState
          pollingFromBlock = BlockNumber $ lastBlock + 1
          pollTo = case filterToBlock fltr of
                     Nothing -> Latest
                     Just bn -> BlockWithNumber bn
      filterId <- Eth.newFilter fltr { filterFromBlock = Just pollingFromBlock }
      void $ reduceEventStream (pollFilter filterId pollTo) handler ()

event :: forall p a . (Provider p, ABIDecode a)
      => Filter
      -> (a -> ReaderT Change (Web3 p) EventAction)
      -> Web3 p ()
event fltr handler = event' fltr 0 handler

mkBlockNumber :: (Provider p) => (Maybe BlockNumber) -> Web3 p BlockNumber
mkBlockNumber (Just toBlock) = return toBlock
mkBlockNumber Nothing        = Eth.blockNumber
