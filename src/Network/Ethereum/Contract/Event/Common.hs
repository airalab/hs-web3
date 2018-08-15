{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE UndecidableInstances   #-}

module Network.Ethereum.Contract.Event.Common  where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import           Control.Exception              (Exception, throwIO)
import           Data.Either                    (lefts, rights)
import           Network.Ethereum.ABI.Event     (DecodeEvent (..))
import qualified Network.Ethereum.Web3.Eth      as Eth
import           Network.Ethereum.Web3.Provider (Web3)
import           Network.Ethereum.Web3.Types    (Change (..), DefaultBlock (..),
                                                 Filter (..), Quantity)

-- | Event callback control response
data EventAction = ContinueEvent
                 -- ^ Continue to listen events
                 | TerminateEvent
                 -- ^ Terminate event listener
  deriving (Show, Eq)


data FilterChange a =
  FilterChange { filterChangeRawChange :: Change
               , filterChangeEvent     :: a
               }

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


-- | Coerce a 'DefaultBlock' into a numerical block number.
mkBlockNumber :: DefaultBlock -> Web3 Quantity
mkBlockNumber bm = case bm of
  BlockWithNumber bn -> return bn
  Earliest           -> return 0
  _                  -> Eth.blockNumber

matchRelevantChanges
  :: DecodeEvent i ni e
  => Filter e
  -> [Change]
  -> IO [FilterChange e]
matchRelevantChanges fltr changes =
    let relevantChanges = filter (\c -> matchesAddress fltr c && matchesTopics fltr c ) changes
    -- in rights $ map (\c@Change{..} -> FilterChange c <$> decodeEvent c) relevantChanges
    in mkFilterChanges relevantChanges
  where
    matchesTopics :: Filter e -> Change -> Bool
    matchesTopics f c =
        case filterTopics f of
          Nothing -> True
          Just ts -> matchTops ts (changeTopics c)
      where
        matchTops [] [] = True
        matchTops (a:as) (b:bs) =
          case a of
            Just a' -> a' == b && matchTops as bs
            Nothing -> matchTops as bs
        matchTops _ _ = False
    matchesAddress :: Filter e -> Change -> Bool
    matchesAddress f c =
      case filterAddress f of
        Nothing -> True
        Just as -> changeAddress c `elem` as

pollTillBlockProgress
  :: Quantity
  -> Web3 Quantity
pollTillBlockProgress currentBlock = do
  bn <- Eth.blockNumber
  if currentBlock >= bn
    then do
      liftIO $ threadDelay 1000000
      pollTillBlockProgress currentBlock
       else pure bn
