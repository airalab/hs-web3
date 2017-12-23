{-# LANGUAGE RecordWildCards #-}

module Network.Ethereum.Web3.Encoding.Generic where

import Data.Int (Int64)
import qualified Data.List as L
import Data.Monoid
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Builder (Builder, toLazyText)
import Data.Attoparsec.Text.Lazy (Parser)
import Network.Ethereum.Web3.Encoding (toDataBuilder)

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
