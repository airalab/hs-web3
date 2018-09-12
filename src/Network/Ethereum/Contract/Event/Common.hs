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

import           Control.Concurrent             (threadDelay)
import           Control.Exception              (Exception, throwIO)
import           Control.Monad.IO.Class         (liftIO)
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
                    , fssLag           :: Integer
                    }


-- | Coerce a 'DefaultBlock' into a numerical block number.
mkBlockNumber :: DefaultBlock -> Web3 Quantity
mkBlockNumber bm = case bm of
  BlockWithNumber bn -> return bn
  Earliest           -> return 0
  _                  -> Eth.blockNumber

{-
 what we need here is that whatever block number 'bn' is returned, that
 1. currentBlock <= bn
 2. bn <= chainHead - lag
 The function clearly satisfies (1) and (2), so we just need to prove that it does return.
 PROOF:
 let chainHead_t1 be the chainHead when the function runs.
 Assume currentBlock <= chainHead_t1 - lag:
   - then maxNextBlock === chainHead_t1 - lag has (1) by the assumption and (2) by tautology
 Otherwise we assume currentBlock > chainHead_t1 - lag:
   - then we loop. At some point in time t2, chainHead_t2 > chainHead_t1 occurs.
     if currentBlock <= chainHead_t2 - lag then we are done by part (i).
     otherwise we continue the loop. At somepoint tk we have curentBlock <= chainHead_tk - lag,
     since chainHead_t1 - lag < chainHead_t2 - lag < .. < chainHead_tk - lag is an increasing sequence.

For example:
 - at first call currentBlock = 11, chainHead = 10, lag  = 5
 - maxNextBlock = 10 - 5 = 5
 - 

-}
pollTillBlockProgress
  :: Quantity
  -> Integer
  -> Web3 Quantity
pollTillBlockProgress currentBlock lag = do
  chainHead <- Eth.blockNumber
  let maxNextBlock = chainHead - fromInteger lag
  if currentBlock <= maxNextBlock
    then pure maxNextBlock
    else do
      liftIO $ threadDelay 3000000
      pollTillBlockProgress currentBlock lag
