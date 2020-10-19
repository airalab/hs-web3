{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Codec.Scale.Test.CoreSpec
-- Copyright   :  Aleksandr Krupenkin 2016-2020
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- Ported to Haskell rust test spec:
-- https://github.com/paritytech/parity-scale-codec/blob/master/tests/single_field_struct_encoding.rs
--

module Codec.Scale.Test.CoreSpec where

import           Control.Monad         (forM_)
import           Data.Bit              (castFromWords8)
import           Data.Bits             (bit)
import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as BS (length, pack, unpack)
import           Data.Int              (Int16, Int32, Int64, Int8)
import qualified Data.Text             as T (pack, unpack)
import           Data.Vector.Unboxed   (Vector)
import qualified Data.Vector.Unboxed   as V (fromList)
import           Data.Word             (Word16, Word32, Word64, Word8)
import qualified GHC.Generics          as GHC (Generic)
import           Test.Hspec
import           Test.Hspec.QuickCheck

import           Codec.Scale

data Unit = Unit
    deriving (Eq, Show, GHC.Generic, Generic, Encode, Decode)

data Indexed = Indexed Word32 Word64
    deriving (Eq, Show, GHC.Generic, Generic, Encode, Decode)

data Struct a b c = Struct
    { _a :: a
    , _b :: b
    , _c :: c
    }
    deriving (Eq, Show, GHC.Generic, Generic, Encode, Decode)

data StructWithPhantom a = StructWithPhantom
    { a1 :: Word32
    , b1 :: Word64
    }
    deriving (Eq, Show, GHC.Generic, Generic, Encode, Decode)

type TestType = Struct Word32 Word64 (Vector Word8);

data EnumType = A
    | B Word32 Word64
    | C
    { a2 :: Word32
    , b2 :: Word64
    }
    deriving (Eq, Show, GHC.Generic, Generic, Encode, Decode)

data TestHasCompact a = TestHasCompact
    { bar1 :: Compact a
    }
    deriving (Eq, Show, GHC.Generic, Generic, Encode, Decode)

data TestHasCompactEnum a = Unnamed (Compact a)
    | Named
    { bar2 :: Compact a
    }
    | UnnamedCompact (Compact a)
    | NamedCompact
    { bar3 :: Compact a
    }
    deriving (Eq, Show, GHC.Generic, Generic, Encode, Decode)

data TestGenericEnum = UnnamedGenericEnum (Compact Integer) (Vector Word16)
    | NamedGenericEnum
    { bar5 :: Compact Word64
    , bar6 :: Word32
    }
    deriving (Eq, Show, GHC.Generic, Generic)

data RecursiveVariant1 a = RecursiveVariant1
    { payload1 :: a
    , other1   :: [RecursiveVariant1 a]
    }
    deriving (Eq, Show, GHC.Generic, Generic, Encode, Decode)

data RecursiveVariant2 a b n = RecursiveVariant2
    { payload2 :: n
    , other2   :: [Struct a b (RecursiveVariant1 n)]
    }
    deriving (Eq, Show, GHC.Generic, Generic, Encode, Decode)

spec :: Spec
spec = parallel $ do
    describe "Regular types" $ do
        prop "Bool" $ \(v :: Bool) -> decode (encode v :: ByteString) == Right v

        prop "Option<Bool>" $ \(v :: Maybe Bool) -> decode (encode v :: ByteString) == Right v

        prop "Result<Bool, Vector<u8>>" $ \v vec -> do
            let success = Right v :: Either (Vector Word8) Bool
                failure = Left (V.fromList vec) :: Either (Vector Word8) Bool
            decode (encode success :: ByteString) == Right success
                && decode (encode failure :: ByteString) == Right failure

        prop "u64" $ \(v :: Word64) -> decode (encode v :: ByteString) == Right v
        prop "u32" $ \(v :: Word32) -> decode (encode v :: ByteString) == Right v
        prop "u16" $ \(v :: Word16) -> decode (encode v :: ByteString) == Right v
        prop "u8" $ \(v :: Word8) -> decode (encode v :: ByteString) == Right v

        prop "i64" $ \(v :: Int64) -> decode (encode v :: ByteString) == Right v
        prop "i32" $ \(v :: Int32) -> decode (encode v :: ByteString) == Right v
        prop "i16" $ \(v :: Int16) -> decode (encode v :: ByteString) == Right v
        prop "i8" $ \(v :: Int8) -> decode (encode v :: ByteString) == Right v

        prop "Compact<integer>" $ \(v :: Integer) -> decode (encode (Compact $ abs v) :: ByteString) == Right (Compact $ abs v)
        prop "Compact<u64>" $ \(v :: Word64) -> decode (encode (Compact v) :: ByteString) == Right (Compact v)
        prop "Compact<u32>" $ \(v :: Word32) -> decode (encode (Compact v) :: ByteString) == Right (Compact v)
        prop "Compact<u16>" $ \(v :: Word16) -> decode (encode (Compact v) :: ByteString) == Right (Compact v)
        prop "Compact<u8>" $ \(v :: Word8) -> decode (encode (Compact v) :: ByteString) == Right (Compact v)

        prop "Vector<u64>" $ \(v :: [Word64]) -> decode (encode (V.fromList v) :: ByteString) == Right v
        prop "Vector<u32>" $ \(v :: [Word32]) -> decode (encode (V.fromList v) :: ByteString) == Right v
        prop "Vector<u16>" $ \(v :: [Word16]) -> decode (encode (V.fromList v) :: ByteString) == Right v
        prop "Vector<u8>" $ \(v :: [Word8]) -> decode (encode (V.fromList v) :: ByteString) == Right v

        prop "BitVec" $ \(v :: [Word8]) -> decode (encode $ castFromWords8 $ V.fromList v :: ByteString) == Right v

        prop "ByteString" $ \(v :: [Word8]) -> decode (encode (BS.pack v) :: ByteString) == Right (BS.pack v)

        prop "Text" $ \(v :: String) -> decode (encode (T.pack v) :: ByteString) == Right (T.pack v)

        prop "List<u64>" $ \(v :: [Word64]) -> decode (encode v :: ByteString) == Right v
        prop "List<u32>" $ \(v :: [Word32]) -> decode (encode v :: ByteString) == Right v
        prop "List<u16>" $ \(v :: [Word16]) -> decode (encode v :: ByteString) == Right v
        prop "List<u8>" $ \(v :: [Word8]) -> decode (encode v :: ByteString) == Right v

    describe "Generic types" $ do
        prop "unamed_enum" $ \v vec ->
            let e = UnnamedGenericEnum (Compact $ abs v) (V.fromList vec)
             in decode' (encode' e :: ByteString) == Right e
        prop "named_struct_enum" $ \a b ->
            let e = NamedGenericEnum (Compact a) b
             in decode' (encode' e :: ByteString) == Right e

    describe "Recursive types" $ do
        prop "variant_1" $ \(n :: Word32) ->
            let v = RecursiveVariant1 n [RecursiveVariant1 (n+1) []]
             in decode (encode v :: ByteString) == Right v

        prop "variant_2" $ \(a :: Word8) (b :: Word16) (n :: Word32) ->
            let v = RecursiveVariant2 n [Struct a b (RecursiveVariant1 (n+1) [])]
             in decode (encode v :: ByteString) == Right v

    describe "SCALE Rust core tests" $ do
        it "option_excheption_works" $ do
            encode (Nothing :: Maybe Bool) `shouldBe` ("\0" :: ByteString)
            encode (Just False) `shouldBe` ("\x01" :: ByteString)
            encode (Just True) `shouldBe` ("\x02" :: ByteString)

        it "should_work_for_simple_enum" $ do
            -- Index modificator isn't support yet, skip codec test for A
            let sb = B 1 2
                sc = C 1 2
                encoded_b = "\x01\x01\0\0\0\x02\0\0\0\0\0\0\0" :: ByteString
                encoded_c = "\x02\x01\0\0\0\x02\0\0\0\0\0\0\0" :: ByteString

            encode sc `shouldBe` encoded_c
            encode sb `shouldBe` encoded_b

            decode encoded_b `shouldBe` Right sb
            decode encoded_c `shouldBe` Right sc
            decode ("\x0a" :: ByteString) `shouldBe` (Left "Failed reading: wrong prefix during enum decoding\nEmpty call stack\n" :: Either String EnumType)

        it "should_derive_encode" $ do
            let v :: TestType
                v = Struct 15 9 (V.fromList $ BS.unpack "Hello world")
                v_encoded :: ByteString
                v_encoded = "\x0f\0\0\0\x09\0\0\0\0\0\0\0\x2cHello world"
            encode v `shouldBe` v_encoded
            Right v `shouldBe` decode v_encoded

        it "should_work_for_unit" $ do
            encode Unit `shouldBe` ("" :: ByteString)
            decode ("" :: ByteString) `shouldBe` Right Unit

        it "should_work_for_indexed" $ do
            let v = Indexed 1 2
                v_encoded :: ByteString
                v_encoded = "\x01\0\0\0\x02\0\0\0\0\0\0\0"
            encode v `shouldBe` v_encoded
            Right v `shouldBe` decode v_encoded

        it "correct_error_for_indexed_0" $ do
            let wrong = "\x08" :: ByteString
            decode wrong `shouldBe` (Left "too few bytes\nFrom:\tdemandInput\n\n" :: Either String Indexed)

        it "correct_error_for_indexed_1" $ do
            let wrong = "\0\0\0\0\x01" :: ByteString
            decode wrong `shouldBe` (Left "too few bytes\nFrom:\tdemandInput\n\n" :: Either String Indexed)

        it "correct_error_for_enumtype" $ do
            let wrong = "\x01" :: ByteString
            decode wrong `shouldBe` (Left "too few bytes\nFrom:\tdemandInput\n\n" :: Either String EnumType)

        it "correct_error_for_named_struct_1" $ do
            let wrong = "\x01" :: ByteString
            decode wrong `shouldBe` (Left "too few bytes\nFrom:\tdemandInput\n\n" :: Either String TestType)

        it "correct_error_for_named_struct_2" $ do
            let wrong = "\0\0\0\0\x01" :: ByteString
            decode wrong `shouldBe` (Left "too few bytes\nFrom:\tdemandInput\n\n" :: Either String TestType)

        let u64_TEST_COMPACT_VALUES :: [(Word64, Int)]
            u64_TEST_COMPACT_VALUES =
                [ (0, 1), (63, 1), (64, 2), (16383, 2)
                , (16384, 4), (1073741823, 4), (1073741824, 5)
                , (bit 32 - 1, 5), (bit 32, 6), (bit 40, 7)
                , (bit 48, 8), (bit 56 - 1, 8), (bit 56, 9)
                , (maxBound, 9)
                ]

        it "compact_works" $ forM_ u64_TEST_COMPACT_VALUES $ \(n, l) -> do
            let encoded = encode (TestHasCompact $ Compact n)
            BS.length encoded `shouldBe` l
            decode encoded `shouldBe` Right (Compact n)

        let u64_TEST_COMPACT_VALUES_FOR_ENUM :: [(Word64, Int)]
            u64_TEST_COMPACT_VALUES_FOR_ENUM =
                [ (0, 2), (63, 2), (64, 3), (16383, 3), (16384, 5)
                , (1073741823, 5), (1073741824, 6), (bit 32 - 1, 6)
                , (bit 32, 7), (bit 40, 8), (bit 48, 9), (bit 56 - 1, 9)
                , (bit 56, 10), (maxBound, 10)
                ]

        it "enum_compact_works" $ forM_ u64_TEST_COMPACT_VALUES_FOR_ENUM $ \(x, l) -> do
            let u = encode $ Unnamed (Compact x)
            BS.length u `shouldBe` l
            decode u `shouldBe` Right (Unnamed $ Compact x)

            let n = encode $ Named (Compact x)
            BS.length n `shouldBe` l
            decode n `shouldBe` Right (Named $ Compact x)

            let uc = encode $ UnnamedCompact (Compact x)
            BS.length uc `shouldBe` l
            decode uc `shouldBe` Right (UnnamedCompact $ Compact x)

            let nc = encode $ NamedCompact (Compact x)
            BS.length nc `shouldBe` l
            decode nc `shouldBe` Right (NamedCompact $ Compact x)
