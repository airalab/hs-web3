{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE UndecidableInstances   #-}

-- |
-- Module      :  Network.Ethereum.Contract.Event.Common
-- Copyright   :  Alexander Krupenkin 2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- Common event manipulation functions.
--

module Network.Ethereum.Contract.Event.Common  where

import           Control.Concurrent            (threadDelay)
import           Control.Exception             (Exception, throwIO)
import           Control.Monad.IO.Class        (liftIO)
import           Data.Either                   (lefts, rights)
import           Data.Solidity.Event           (DecodeEvent (..))
import qualified Network.Ethereum.Api.Eth      as Eth
import           Network.Ethereum.Api.Provider (Web3)
import           Network.Ethereum.Api.Types    (Change (..), DefaultBlock (..),
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


pollTillBlockProgress
  :: Quantity
  -> Web3 Quantity
pollTillBlockProgress currentBlock = do
  bn <- Eth.blockNumber
  if currentBlock >= bn
    then do
      liftIO $ threadDelay 3000000
      pollTillBlockProgress currentBlock
       else pure bn
