{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeInType          #-}
{-# LANGUAGE TypeOperators       #-}

-- |
-- Module      :  Network.Ethereum.ABI.Generic
-- Copyright   :  Alexander Krupenkin 2016-2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  noportable
--
-- This module is internal, the purpose is to define helper classes and functions
-- to assist in encoding and decoding Solidity types for function calls and events.
-- The user of this library should have no need to use this directly in application code.
--

module Network.Ethereum.ABI.Generic () where

import qualified Data.ByteString.Lazy          as LBS
import           Data.Int                      (Int64)
import qualified Data.List                     as L
import           Data.Monoid                   ((<>))
import           Data.Proxy                    (Proxy (..))
import           Data.Serialize                (Get, Put)
import           Data.Serialize.Get            (bytesRead, lookAheadE, skip)
import           Data.Serialize.Put            (runPutLazy)
import           Generics.SOP                  (I (..), NP (..), NS (..),
                                                SOP (..))

import           Network.Ethereum.ABI.Class    (ABIGet (..), ABIPut (..),
                                                ABIType (..),
                                                GenericABIGet (..),
                                                GenericABIPut (..))
import           Network.Ethereum.ABI.Prim.Int (getWord256, putWord256)

data EncodedValue =
  EncodedValue { order    :: Int64
               , offset   :: Maybe Int64
               , encoding :: Put
               }

instance Eq EncodedValue where
  ev1 == ev2 = order ev1 == order ev2

instance Ord EncodedValue where
  compare ev1 ev2 = order ev1 `compare` order ev2

combineEncodedValues :: [EncodedValue] -> Put
combineEncodedValues encodings =
  let sortedEs = adjust headsOffset $ L.sort encodings
      encodings' = addTailOffsets headsOffset [] sortedEs
  in let heads = foldl (\acc EncodedValue{..} -> case offset of
                          Nothing -> acc <> encoding
                          Just o  -> acc <> putWord256 (fromIntegral o)
                      ) mempty encodings'
         tails = foldl (\acc EncodedValue{..} -> case offset of
                          Nothing -> acc
                          Just _  -> acc <> encoding
                      ) mempty encodings'
      in heads <> tails
  where
    adjust :: Int64 -> [EncodedValue] -> [EncodedValue]
    adjust n = map (\ev -> ev {offset = (+) n <$> offset ev})
    addTailOffsets :: Int64 -> [EncodedValue] -> [EncodedValue] -> [EncodedValue]
    addTailOffsets init' acc es = case es of
      [] -> reverse acc
      (e : tail') -> case offset e of
        Nothing -> addTailOffsets init' (e : acc) tail'
        Just _  -> addTailOffsets init' (e : acc) (adjust (LBS.length . runPutLazy . encoding $ e) tail')
    headsOffset :: Int64
    headsOffset = foldl (\acc e -> case offset e of
                                Nothing -> acc + (LBS.length . runPutLazy . encoding $ e)
                                Just _ -> acc + 32
                            ) 0 encodings

class ABIData a where
    _serialize :: [EncodedValue] -> a -> [EncodedValue]

instance ABIData (NP f '[]) where
    _serialize encoded _ = encoded

instance (ABIType b, ABIPut b, ABIData (NP I as)) => ABIData (NP I (b :as)) where
    _serialize encoded (I b :* a) =
        if isDynamic (Proxy :: Proxy b)
        then _serialize (dynEncoding  : encoded) a
        else _serialize (staticEncoding : encoded) a
      where
        staticEncoding = EncodedValue { encoding = abiPut b
                                      , offset = Nothing
                                      , order = 1 + (fromInteger . toInteger . L.length $ encoded)
                                      }
        dynEncoding = EncodedValue { encoding = abiPut b
                                   , offset = Just 0
                                   , order = 1 + (fromInteger . toInteger . L.length $ encoded)
                                   }

instance ABIData (NP f as) => GenericABIPut (SOP f '[as]) where
    gAbiPut (SOP (Z a)) = combineEncodedValues $ _serialize [] a
    gAbiPut _           = error "Impossible branch"

instance GenericABIGet (NP f '[]) where
    gAbiGet = return Nil

instance (ABIGet a, GenericABIGet (NP I as)) => GenericABIGet (NP I (a : as)) where
    gAbiGet = (:*) <$> (I <$> factorParser) <*> gAbiGet

instance GenericABIGet (NP f as) => GenericABIGet (SOP f '[as]) where
    gAbiGet = SOP . Z <$> gAbiGet

factorParser :: forall a . ABIGet a => Get a
factorParser
  | not $ isDynamic (Proxy :: Proxy a) = abiGet
  | otherwise = do
        dataOffset <- fromIntegral <$> getWord256
        currentOffset <- bytesRead
        Left x <- lookAheadE $ do
            skip (dataOffset - currentOffset)
            Left <$> abiGet
        return x
