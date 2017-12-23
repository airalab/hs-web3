{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Ethereum.Web3.Encoding.Generic where

import Data.Int (Int64)
import qualified Data.List as L
import Data.Monoid
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Builder (Builder, toLazyText)
import Data.Attoparsec.Text.Lazy (Parser)
import Generics.SOP
import Network.Ethereum.Web3.Encoding (ABIEncode(..), ABIDecode(..))
import Network.Ethereum.Web3.Encoding.Internal (EncodingType(..))

-- | A class for encoding generically composed datatypes to their abi encoding
class GenericABIEncode a where
  genericToDataBuilder :: a -> Builder

-- | A class for decoding generically composed datatypes from their abi encoding
class GenericABIDecode a where
  genericFromDataParser :: Parser a

data EncodedValue =
  EncodedValue { order :: Int64
               , offset :: Maybe Int64
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
                          Just o -> acc <> toDataBuilder (toInteger o)
                      ) mempty encodings'
         tails = foldl (\acc EncodedValue{..} -> case offset of
                          Nothing -> acc
                          Just _ -> acc <> encoding
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
        Just _ -> addTailOffsets init (e : acc) (adjust ((T.length . toLazyText . encoding $ e) `div` 2) tail)
    headsOffset :: Int64
    headsOffset = foldl (\acc e -> case offset e of
                                Nothing -> acc + ((T.length . toLazyText . encoding $ e) `div` 2)
                                Just _ -> acc + 32
                            ) 0 encodings

class ABIData a where
    _serialize :: [EncodedValue] -> a -> [EncodedValue]

instance ABIData (NP f '[]) where
    _serialize encoded _ = encoded


instance (EncodingType b, ABIEncode b, ABIData (NP I as)) => ABIData (NP I (b :as)) where
  _serialize encoded (I b :* a) =
    if isDynamic (undefined :: b)
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
