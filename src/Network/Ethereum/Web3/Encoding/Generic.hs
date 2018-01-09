{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeInType          #-}
{-# LANGUAGE TypeOperators       #-}


-- |
-- Module      :  Network.Ethereum.Web3.Encoding.Generic
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- This module is internal, the purpose is to define helper classes and functions
-- to assist in encoding and decoding Solidity types for function calls and events.
-- The user of this library should have no need to use this directly in application code.
--

module Network.Ethereum.Web3.Encoding.Generic (
    GenericABIEncode
  , GenericABIDecode
  , genericABIEncode
  , genericToData
  , genericABIDecode
  , genericFromData
  , Singleton(..)
  ) where

import           Control.Error
import           Data.Int                                (Int64)
import qualified Data.List                               as L
import           Data.Monoid
import           Data.Proxy                              (Proxy (..))
import           Data.Tagged                             (Tagged (..))
import qualified Data.Text                               as T
import qualified Data.Text.Lazy                          as LT
import           Data.Text.Lazy.Builder                  (Builder, toLazyText)
import           Generics.SOP                            (Generic (..), I (..),
                                                          NP (..), NS (..),
                                                          Rep (..), SOP (..))
import qualified GHC.Generics                            as GHC (Generic)
import           Text.Parsec                             (getPosition,
                                                          lookAhead, parse,
                                                          sourceColumn)
import           Text.Parsec.Text.Lazy                   (Parser)

import           Network.Ethereum.Web3.Encoding          (ABIDecode (..),
                                                          ABIEncode (..))
import           Network.Ethereum.Web3.Encoding.Internal (EncodingType (..),
                                                          takeHexChar)

-- | A class for encoding generically composed datatypes to their abi encoding
class GenericABIEncode a where
  genericToDataBuilder :: a -> Builder

-- | A class for decoding generically composed datatypes from their abi encoding
class GenericABIDecode a where
  genericFromDataParser :: Parser a

data EncodedValue =
  EncodedValue { order    :: Int64
               , offset   :: Maybe Int64
               , encoding :: Builder
               }

instance Eq EncodedValue where
  ev1 == ev2 = order ev1 == order ev2

instance Ord EncodedValue where
  compare ev1 ev2 = order ev1 `compare` order ev2

combineEncodedValues :: [EncodedValue] -> Builder
combineEncodedValues encodings =
  let sortedEs = adjust headsOffset $ L.sort encodings
      encodings' = addTailOffsets headsOffset [] sortedEs
  in let heads = foldl (\acc EncodedValue{..} -> case offset of
                          Nothing -> acc <> encoding
                          Just o  -> acc <> toDataBuilder (toInteger o)
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
    addTailOffsets init acc es = case es of
      [] -> reverse acc
      (e : tail) -> case offset e of
        Nothing -> addTailOffsets init (e : acc) tail
        Just _ -> addTailOffsets init (e : acc) (adjust ((LT.length . toLazyText . encoding $ e) `div` 2) tail)
    headsOffset :: Int64
    headsOffset = foldl (\acc e -> case offset e of
                                Nothing -> acc + ((LT.length . toLazyText . encoding $ e) `div` 2)
                                Just _ -> acc + 32
                            ) 0 encodings

class ABIData a where
    _serialize :: [EncodedValue] -> a -> [EncodedValue]

instance ABIData (NP f '[]) where
    _serialize encoded _ = encoded

instance (EncodingType b, ABIEncode b, ABIData (NP I as)) => ABIData (NP I (b :as)) where
  _serialize encoded (I b :* a) =
    if isDynamic (Proxy :: Proxy b)
       then _serialize (dynEncoding  : encoded) a
       else _serialize (staticEncoding : encoded) a
    where
      staticEncoding = EncodedValue { encoding = toDataBuilder b
                                    , offset = Nothing
                                    , order = 1 + (fromInteger . toInteger . L.length $ encoded)
                                    }
      dynEncoding = EncodedValue { encoding = toDataBuilder b
                                 , offset = Just 0
                                 , order = 1 + (fromInteger . toInteger . L.length $ encoded)
                                 }

instance ABIData (NP f as) => GenericABIEncode (SOP f '[as]) where
  genericToDataBuilder (SOP (Z a)) = combineEncodedValues $ _serialize [] a

genericABIEncode :: ( Generic a
                    , Rep a ~ rep
                    , GenericABIEncode rep
                    )
                 => a
                 -> Builder
genericABIEncode = genericToDataBuilder . from

genericToData :: ( Generic a
                 , Rep a ~ rep
                 , GenericABIEncode rep
                 )
              => a
              -> T.Text
genericToData = LT.toStrict . toLazyText . genericToDataBuilder . from

instance GenericABIDecode (NP f '[]) where
  genericFromDataParser = return Nil

instance (EncodingType a, ABIDecode a, GenericABIDecode (NP I as)) => GenericABIDecode (NP I (a: as)) where
  genericFromDataParser = (:*) <$> (I <$> factorParser) <*> genericFromDataParser

instance GenericABIDecode (NP f as) => GenericABIDecode (SOP f '[as]) where
  genericFromDataParser = SOP . Z <$> genericFromDataParser

genericABIDecode :: ( Generic a
                    , Rep a ~ rep
                    , GenericABIDecode rep
                    ) => Parser a
genericABIDecode = to <$> genericFromDataParser

genericFromData :: ( Generic a
                   , Rep a ~ rep
                   , GenericABIDecode rep
                   )
                => T.Text
                -> Maybe a
genericFromData = fmap to . hush . parse genericFromDataParser "" . LT.fromStrict

factorParser :: forall a . ABIDecode a => EncodingType a => Parser a
factorParser
  | not $ isDynamic (Proxy :: Proxy a) = fromDataParser
  | otherwise = dParser

dParser :: forall a . ABIDecode a => Parser a
dParser = do
  dataOffset <- fromInteger <$> fromDataParser
  lookAhead $ do
    n <- sourceColumn <$> getPosition
    _ <- takeHexChar (dataOffset * 2 - (n - 1))
    fromDataParser

-- We also need "one-tuples"
newtype Singleton a = Singleton { unSingleton :: a } deriving GHC.Generic

deriving instance Eq a => Eq (Singleton a)
deriving instance Show a => Show (Singleton a)

instance Generic (Singleton a)

instance ABIEncode a => ABIEncode (Singleton a) where
  toDataBuilder (Singleton a) = toDataBuilder a

instance ABIDecode a => ABIDecode (Singleton a) where
  fromDataParser = Singleton <$> fromDataParser
