{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

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
import           Data.Machine                   (Is (Refl), ProcessT,
                                                 Step (Await, Stop, Yield),
                                                 autoT, runMachineT)
import           Data.Machine.MealyT            (MealyT (..), arrM)
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
playLogs :: forall p i ni e.
            ( Provider p
            , DecodeEvent i ni e
            , Event e
            )
         => HaltingMealyT (Web3 p) FilterStreamState [FilterChange e]
playLogs  = do
  filter <- filterStream
  changes <- arrM . const . lift $ Eth.getLogs filter
  return $ mkFilterChanges changes

pollFilter :: forall p i ni e s .
              ( Provider p
              , DecodeEvent i ni e
              , Event e
              )
           => FilterId
           -> DefaultBlock
           -> HaltingMealyT (Web3 p) s [FilterChange e]
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

filterStream :: forall p . Provider p
             => HaltingMealyT (Web3 p) FilterStreamState Filter
filterStream = MealyT $ \FilterStreamState{..} -> do
  end <- lift . mkBlockNumber $ filterToBlock fssInitialFilter
  if fssCurrentBlock > end
  then MaybeT $ return Nothing -- halt
  else return $ let to' = newTo end fssCurrentBlock fssWindowSize
                    filter' = fssInitialFilter { filterFromBlock = BlockWithNumber fssCurrentBlock
                                               , filterToBlock = BlockWithNumber to'
                                               }
                    MealyT next = filterStream
                in (filter', MealyT $ \s -> next s { fssCurrentBlock = succ to' })

      where succ :: BlockNumber -> BlockNumber
            succ (BlockNumber bn) = BlockNumber $ bn + 1

            newTo :: BlockNumber -> BlockNumber -> Integer -> BlockNumber
            newTo upper (BlockNumber current) window = min upper . BlockNumber $ current + window


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
  mLastProcessedFilterState <- reduceEventStream playLogs handler initState
  case mLastProcessedFilterState of
    Nothing -> return ()
    Just lastProcessedFilterState -> do
      let BlockNumber lastBlock = fssCurrentBlock lastProcessedFilterState
          pollingFromBlock = BlockNumber $ lastBlock + 1
          pollTo = filterToBlock fltr
      filterId <- Eth.newFilter fltr { filterFromBlock = BlockWithNumber pollingFromBlock }
      void $ reduceEventStream (pollFilter filterId pollTo) handler ()

event :: forall p i ni e .
         ( Provider p
         , DecodeEvent i ni e
         , Event e
         )
      => Filter
      -> (e -> ReaderT Change (Web3 p) EventAction)
      -> Web3 p (Async ())
event fltr handler = forkWeb3 $ event' fltr 0 handler

mkBlockNumber :: Provider p => DefaultBlock -> Web3 p BlockNumber
mkBlockNumber bm = case bm of
  BlockWithNumber bn -> return bn
  Earliest -> return 0
  _ -> Eth.blockNumber

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
