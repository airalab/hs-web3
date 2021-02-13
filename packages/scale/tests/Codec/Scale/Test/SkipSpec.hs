{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Codec.Scale.Test.SkipSpec
-- Copyright   :  Aleksandr Krupenkin 2016-2021
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- Ported to Haskell rust test spec:
-- https://github.com/paritytech/parity-scale-codec/blob/master/tests/skip.rs
--

module Codec.Scale.Test.SkipSpec where

import           Data.ByteString           (ByteString, pack)
import           Data.Default              (Default)
import           Data.Word                 (Word32)
import qualified GHC.Generics              as GHC (Generic)
import           Test.Hspec

import           Codec.Scale
import           Codec.Scale.SingletonEnum
import           Codec.Scale.Skip

data UncodecType = UncodecType
    deriving (Eq, Ord, Show, GHC.Generic, Default)

-- Implementing A constructor is impossible
data EnumType = B
    { _b1 :: Skip UncodecType
    , b2  :: Word32
    }
    | C (Skip UncodecType) Word32
    deriving (Eq, Ord, Show, GHC.Generic, Generic, Encode, Decode)


data StructNamed = StructNamed
    { a :: Skip UncodecType
    , b :: Word32
    }
    deriving (Eq, Ord, Show, GHC.Generic, Generic, Encode, Decode)

data StructUnnamed = StructUnnamed (Skip UncodecType) Word32
    deriving (Eq, Ord, Show, GHC.Generic, Generic, Encode, Decode)

data NamedStruct = NamedStruct
    { some_named :: Word32
    , ignore     :: Skip (Maybe Word32)
    }
    deriving (Eq, Ord, Show, GHC.Generic, Generic, Encode, Decode)

spec :: Spec
spec = parallel $ do
    describe "Type encoding modificators" $ do
        it "enum_struct_test" $ do
            let eb = B { _b1 = Skip UncodecType, b2 = 1 }
                ec = C (Skip UncodecType) 1
                sn = StructNamed { a = Skip UncodecType, b = 1 }
                su = StructUnnamed (Skip UncodecType) 1

            let eb_encoded = encode eb :: ByteString
            let ec_encoded = encode ec :: ByteString
            let sn_encoded = encode sn :: ByteString
            let su_encoded = encode su :: ByteString

            decode eb_encoded `shouldBe` Right eb
            decode ec_encoded `shouldBe` Right ec
            decode sn_encoded `shouldBe` Right sn
            decode su_encoded `shouldBe` Right su

        it "skip_enum_struct_inner_variant" $ do
            let struct = NamedStruct { some_named = 1, ignore = Skip (Just 1) }
                single = SingletonEnum struct
                encoded = pack [0x00, 0x01, 0x00, 0x00, 0x00]
            encode single `shouldBe` encoded
