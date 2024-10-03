{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeInType          #-}
{-# LANGUAGE TypeOperators       #-}

-- |
-- Module      :  Data.Solidity.Abi.Generic
-- Copyright   :  Aleksandr Krupenkin 2016-2024
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  noportable
--
-- This module is internal, the purpose is to define helper classes and functions
-- to assist in encoding and decoding Solidity types for function calls and events.
-- The user of this library should have no need to use this directly in application code.
--

module Data.Solidity.Abi.Generic () where

import qualified Data.ByteString.Lazy   as LBS
import           Data.Int               (Int64)
import qualified Data.List              as L
import           Data.Proxy             (Proxy (..))
import           Data.Serialize         (Get, Put)
import           Data.Serialize.Get     (bytesRead, lookAheadE, skip)
import           Data.Serialize.Put     (runPutLazy)
import           Generics.SOP           (I (..), NP (..), NS (..), SOP (..))

import           Data.Solidity.Abi      (AbiGet (..), AbiPut (..), AbiType (..),
                                         GenericAbiGet (..), GenericAbiPut (..))
import           Data.Solidity.Prim.Int (getWord256, putWord256)

data EncodedValue = EncodedValue
    { evOrder                 :: Int64
    , evIsDynamic             :: Bool
    , evEncoding              :: Put
    , evEncodingLengthInBytes :: Int64 -- cache
    }

instance Eq EncodedValue where
  ev1 == ev2 = evOrder ev1 == evOrder ev2

instance Ord EncodedValue where
  compare ev1 ev2 = evOrder ev1 `compare` evOrder ev2

-- from https://docs.soliditylang.org/en/v0.8.12/abi-spec.html#examples
--
-- if Ti is static:
--   head(X(i)) = enc(X(i)) and tail(X(i)) = "" (the empty string)
-- otherwise, i.e. if Ti is dynamic:
--   head(X(i)) = enc(len( head(X(1)) ... head(X(k)) tail(X(1)) ... tail(X(i-1)) )) tail(X(i)) = enc(X(i))
combineEncodedValues :: [EncodedValue] -> Put
combineEncodedValues encodings =
  let sortedEncodings = L.sort encodings

      wordLengthInBytes :: Int64
      wordLengthInBytes = 32

      headsOffsetInBytes :: Int64
      headsOffsetInBytes = foldl (+) 0 $ map (\EncodedValue{..} -> if evIsDynamic then wordLengthInBytes else evEncodingLengthInBytes) encodings

      heads = fst $ foldl
        (\(accumulator, lengthOfPreviousDynamicValues) EncodedValue{..} -> if evIsDynamic
            then ( accumulator <> putWord256 (fromIntegral $ headsOffsetInBytes + lengthOfPreviousDynamicValues)
                 , lengthOfPreviousDynamicValues + evEncodingLengthInBytes
                 )
            else ( accumulator <> evEncoding
                 , lengthOfPreviousDynamicValues
                 )
        )
        (mempty, 0)
        sortedEncodings
      tails = foldMap
        (\EncodedValue{..} -> if evIsDynamic
            then evEncoding
            else mempty
        )
        sortedEncodings
      in heads <> tails
  where

-- aIsDynamic is a variable because of https://github.com/airalab/hs-web3/pull/129#issuecomment-1074045478
-- TODO: call the `isDynamic` function in the `mkEncodedValue` function
mkEncodedValue :: (AbiType a, AbiPut a) => [EncodedValue] -> a -> Bool -> EncodedValue
mkEncodedValue otherEncodedArray a aIsDynamic =
  let encoding = abiPut a
  in EncodedValue
  { evEncoding              = encoding
  , evOrder                 = fromInteger . toInteger . L.length $ otherEncodedArray
  , evIsDynamic             = aIsDynamic
  , evEncodingLengthInBytes = lengthInBytes encoding
  }
  where
  lengthInBytes :: Put -> Int64
  lengthInBytes e = LBS.length . runPutLazy $ e

class AbiData a where
    _serialize :: [EncodedValue] -> a -> [EncodedValue]

instance AbiData (NP f '[]) where
    _serialize encoded _ = encoded

instance (AbiType b, AbiPut b, AbiData (NP I as)) => AbiData (NP I (b : as)) where
    _serialize encoded (I b :* a) = _serialize (mkEncodedValue encoded b (isDynamic (Proxy :: Proxy b)) : encoded) a

instance AbiData (NP f as) => GenericAbiPut (SOP f '[as]) where
    gAbiPut (SOP (Z a)) = combineEncodedValues $ _serialize [] a
    gAbiPut _           = error "Impossible branch"

instance GenericAbiGet (NP f '[]) where
    gAbiGet = return Nil

instance (AbiGet a, GenericAbiGet (NP I as)) => GenericAbiGet (NP I (a : as)) where
    gAbiGet = (:*) <$> (I <$> factorParser) <*> gAbiGet

instance GenericAbiGet (NP f as) => GenericAbiGet (SOP f '[as]) where
    gAbiGet = SOP . Z <$> gAbiGet

factorParser :: forall a . AbiGet a => Get a
factorParser
  | not $ isDynamic (Proxy :: Proxy a) = abiGet
  | otherwise = do
        dataOffset <- fromIntegral <$> getWord256
        currentOffset <- bytesRead
        Left x <- lookAheadE $ do
            skip (dataOffset - currentOffset)
            Left <$> abiGet
        return x
