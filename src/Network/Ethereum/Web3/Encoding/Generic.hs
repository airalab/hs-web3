{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Network.Ethereum.Web3.Encoding.Generic (
    genericABIEncode
  , genericABIDecode
  , Singleton(..)
  ) where

import qualified Data.Attoparsec.Text                    as P
import Data.Attoparsec.Combinator (lookAhead)


import Data.Int (Int64)
import qualified Data.List as L
import Data.Monoid
import Data.Proxy (Proxy(..))
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Builder (Builder, toLazyText)
import Data.Attoparsec.Text.Lazy (Parser, maybeResult, parse)
import Generics.SOP (Generic(..), NP(..), NS(..), I(..), SOP(..), Rep(..))
import qualified GHC.Generics as GHC (Generic)

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

instance ABIData (NP f as) => GenericABIEncode (NS (NP f) '[as]) where
  genericToDataBuilder (Z a) = combineEncodedValues $ _serialize [] a

instance ABIData (NS (NP f) as) => GenericABIEncode (SOP f as) where
  genericToDataBuilder (SOP a) = combineEncodedValues $ _serialize [] a

genericABIEncode :: ( Generic a
                    , Rep a ~ rep
                    , GenericABIEncode rep
                    )
                 => a
                 -> Builder
genericABIEncode = genericToDataBuilder . from

{-
genericToData :: ( Generic a
                 , Rep a ~ rep
                 , GenericABIEncode rep
                 )
              => a
              -> T.Text
genericToData = LT.toStrict . toLazyText . genericToDataBuilder . from

-}

instance GenericABIDecode (NP f '[]) where
  genericFromDataParser = return Nil

instance (EncodingType a, ABIDecode a, GenericABIDecode (NP I as)) => GenericABIDecode (NP I (a: as)) where
  genericFromDataParser = (:*) <$> (I <$> factorParser) <*> genericFromDataParser

instance GenericABIDecode (NP f as) => GenericABIDecode (NS (NP f) '[as]) where
  genericFromDataParser = Z <$> genericFromDataParser

instance GenericABIDecode (NS (NP f) as) => GenericABIDecode (SOP f as) where
  genericFromDataParser = SOP <$> genericFromDataParser

genericABIDecode :: ( Generic a
                    , Rep a ~ rep
                    , GenericABIDecode rep
                    ) => Parser a
genericABIDecode = to <$> genericFromDataParser

{-
genericFromData :: ( Generic a
                   , Rep a ~ rep
                   , GenericABIDecode rep
                   )
                => T.Text
                -> Maybe a
genericFromData = fmap to . maybeResult . parse genericFromDataParser . LT.fromStrict
-}

factorParser :: (ABIDecode a, EncodingType a) => Parser a
factorParser = undefined
--  | not $ isDynamic (Proxy :: Proxy a) = fromDataParser
--  | otherwise = undefined

-- | Dynamic argument parser
--dParser :: ABIDecode a => Parser a
--dParser = do
--  dataOffset <- fromDataParser
--  lookAhead $ do
--    _ <- P.take dataOffset
--    fromDataParser


-- We also need "one-tuples"
newtype Singleton a = Singleton { unSingleton :: a } deriving GHC.Generic

instance Generic (Singleton a)

