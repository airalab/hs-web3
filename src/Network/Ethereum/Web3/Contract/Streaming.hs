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

import           Control.Monad                  (forM)
import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.Maybe      (MaybeT (..))
import           Control.Monad.Trans.Reader     (ReaderT (..), runReaderT)
import           Data.Machine                   (Is (Refl), ProcessT,
                                                 Step (Await, Stop, Yield),
                                                 autoT, runMachineT)
import           Data.Machine.MealyT            (MealyT (..))

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

      where mkBlockNumber :: (Maybe BlockNumber) -> Web3 p BlockNumber
            mkBlockNumber (Just toBlock) = return toBlock
            mkBlockNumber Nothing        = Eth.blockNumber

            succ :: BlockNumber -> BlockNumber
            succ (BlockNumber bn) = BlockNumber $ bn + 1

            newTo :: BlockNumber -> BlockNumber -> Integer -> BlockNumber
            newTo upper (BlockNumber current) window = min upper . BlockNumber $ current + window
