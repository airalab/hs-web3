{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

-- |
-- Module      :  Codec.Scale.Test.SingleFieldStructSpec
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

module Codec.Scale.Test.SingleFieldStructSpec where

import           Data.ByteString  as BS (pack)
import           Data.Default     (def)
import           Data.Word        (Word32, Word64)
import           Generics.SOP     (Generic)
import qualified GHC.Generics     as GHC (Generic)
import           Test.Hspec

import           Codec.Scale
import           Codec.Scale.Skip

newtype S = S { x1 :: Word32 }
    deriving (Eq, Ord, Show, Encode, Decode)

data SSkip = SSkip
    { s1 :: Skip Word32
    , x2 :: Word32
    , s2 :: Skip Word32
    }
    deriving (Eq, Ord, Show, GHC.Generic)

instance Generic SSkip
instance Encode SSkip
instance Decode SSkip

newtype Sc = Sc { x3 :: Compact Word32 }
    deriving (Eq, Ord, Show, Encode, Decode)

newtype U = U Word32
    deriving (Eq, Ord, Show, Enum, Num, Real, Integral, Encode, Decode)

newtype U2 = U2 { a :: Word64 }
    deriving (Eq, Ord, Show, Encode, Decode)

data USkip = USkip (Skip Word32) Word32 (Skip Word32)
    deriving (Eq, Ord, Show, GHC.Generic)

instance Generic USkip
instance Encode USkip
instance Decode USkip

newtype Uc = Uc (Compact Word32)
    deriving (Eq, Ord, Show, Encode, Decode)

newtype Ucas = Ucas (Compact U)
    deriving (Eq, Ord, Show, Encode, Decode)

spec :: Spec
spec = parallel $ do
    describe "Single field struct encoding" $ do
        let x = 3
            s = S x
            s_skip = SSkip def x def
            sc = Sc (Compact x)
            u = U x
            u_skip = USkip def x def
            uc = Uc (Compact x)
            ucom = Compact u
            ucas = Ucas (Compact u)

            s_encoded = BS.pack [3, 0, 0, 0]
            s_skip_encoded = BS.pack [3, 0, 0, 0]
            sc_encoded = BS.pack [12]
            u_encoded = BS.pack [3, 0, 0, 0]
            u_skip_encoded = BS.pack [3, 0, 0, 0]
            uc_encoded = BS.pack [12]
            ucom_encoded = BS.pack [12]
            ucas_encoded = BS.pack [12]

        it "encoding" $ do
            encode s `shouldBe` s_encoded
            encode s_skip `shouldBe` s_skip_encoded
            encode sc `shouldBe` sc_encoded
            encode u `shouldBe` u_encoded
            encode u_skip `shouldBe` u_skip_encoded
            encode uc `shouldBe` uc_encoded
            encode ucom `shouldBe` ucom_encoded
            encode ucas `shouldBe` ucas_encoded

        it "decoding" $ do
            Right s `shouldBe` decode s_encoded
            Right s_skip `shouldBe` decode s_skip_encoded
            Right sc `shouldBe` decode sc_encoded
            Right u `shouldBe`  decode u_encoded
            Right u_skip `shouldBe` decode u_skip_encoded
            Right uc `shouldBe` decode uc_encoded
            Right ucom `shouldBe` decode ucom_encoded
            Right ucas `shouldBe` decode ucas_encoded
