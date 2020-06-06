{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- |
-- Module      :  Codec.Scale.Core
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- Particular core type instances.
--

module Codec.Scale.Core (Compact(..)) where

import           Control.Monad       (replicateM)
import           Data.Bit            (Bit, castFromWords8, cloneToWords8)
import           Data.Int            (Int16, Int32, Int64, Int8)
import           Data.Serialize.Get  (getInt16le, getInt32le, getInt64le,
                                      getInt8, getWord16le, getWord32le,
                                      getWord64le, getWord8)
import           Data.Serialize.Put  (putInt16le, putInt32le, putInt64le,
                                      putInt8, putWord16le, putWord32le,
                                      putWord64le, putWord8)
import           Data.Vector.Unboxed (Unbox, Vector)
import qualified Data.Vector.Unboxed as V
import           Data.Word           (Word16, Word32, Word64, Word8)
import           Generics.SOP        ()

import           Codec.Scale.Class   (Decode (..), Encode (..))
import           Codec.Scale.Compact (Compact (..))
import           Codec.Scale.Generic ()
import           Codec.Scale.TH      (tupleInstances)

--
-- Boolean instance.
--

instance Encode Bool where
    put False = putWord8 0
    put True  = putWord8 1

instance Decode Bool where
    get = do x <- getWord8
             case x of
               0 -> return False
               1 -> return True
               _ -> fail "invalid boolean representation"

--
-- Integer instances.
--

instance Encode Word8 where
    put = putWord8

instance Decode Word8 where
    get = getWord8

instance Encode Word16 where
    put = putWord16le

instance Decode Word16 where
    get = getWord16le

instance Encode Word32 where
    put = putWord32le

instance Decode Word32 where
    get = getWord32le

instance Encode Word64 where
    put = putWord64le

instance Decode Word64 where
    get = getWord64le

instance Encode Int8 where
    put = putInt8

instance Decode Int8 where
    get = getInt8

instance Encode Int16 where
    put = putInt16le

instance Decode Int16 where
    get = getInt16le

instance Encode Int32 where
    put = putInt32le

instance Decode Int32 where
    get = getInt32le

instance Encode Int64 where
    put = putInt64le

instance Decode Int64 where
    get = getInt64le

--
-- Option type instances.
--

-- Let's map `Maybe a` type to Rust `Option<T>`: Just -> Some, Nothing -> None

instance Encode a => Encode (Maybe a) where
    put (Just a) = putWord8 1 >> put a
    put Nothing  = putWord8 0

instance Decode a => Decode (Maybe a) where
    get = do
        x <- getWord8
        case x of
          0 -> return Nothing
          1 -> Just <$> get
          _ -> fail "unexpecded first byte decoding Option"

-- Option<bool> is exception and it is always one byte

instance {-# OVERLAPPING #-} Encode (Maybe Bool) where
    put Nothing      = putWord8 0
    put (Just False) = putWord8 1
    put (Just True)  = putWord8 2

instance {-# OVERLAPPING #-} Decode (Maybe Bool) where
    get = do
        x <- getWord8
        case x of
          0 -> return Nothing
          1 -> return (Just False)
          2 -> return (Just True)
          _ -> fail "unexpecded first byte decoding OptionBool"

--
-- Result type isntances.
--

-- Let's map `Ether a b` type to Rust `Result<T, E>`: Left -> Error, Right -> Ok

instance (Encode a, Encode b) => Encode (Either a b) where
    put (Right a) = putWord8 0 >> put a
    put (Left a)  = putWord8 1 >> put a

instance (Decode a, Decode b) => Decode (Either a b) where
    get = do
        x <- getWord8
        case x of
          0 -> Right <$> get
          1 -> Left <$> get
          _ -> fail "unexpected first byte decoding Result"


--
-- Tuple type instances.
--

$(concat <$> mapM tupleInstances [2..20])

--
-- Vector type instances.
--

instance Encode a => Encode [a] where
    put list = do
        put (Compact $ length list)
        mapM_ put list

instance Decode a => Decode [a] where
    get = do
        len <- get
        replicateM (unCompact len) get

instance (Encode a, Unbox a) => Encode (Vector a) where
    put vec = do
        put (Compact $ V.length vec)
        V.mapM_ put vec

instance (Decode a, Unbox a) => Decode (Vector a) where
    get = do
        len <- get
        V.replicateM (unCompact len) get

instance Encode (Vector Bit) where
    put vec = do
        let encoded = cloneToWords8 vec
        put (Compact $ V.length encoded)
        V.mapM_ put encoded

instance Decode (Vector Bit) where
    get = do
        len <- get
        castFromWords8 <$> V.replicateM (unCompact len) get
