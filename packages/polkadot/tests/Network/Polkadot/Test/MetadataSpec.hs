{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

-- |
-- Module      :  Codec.Scale.Test.CoreSpec
-- Copyright   :  Aleksandr Krupenkin 2016-2020
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- Polkadot metadata tests ported from:
-- https://github.com/polkadot-js/api/tree/master/packages/metadata/src/Metadata
--

module Network.Polkadot.Test.MetadataSpec where

import           Codec.Scale                           (decode)
import           Data.ByteArray.HexString              (HexString, hexFrom)
import           Network.Polkadot.Metadata
import           Network.Polkadot.Metadata.MagicNumber
import           Network.Polkadot.Metadata.V9
import           Test.Hspec

spec :: Spec
spec = parallel $ do
    describe "Metadata magic" $ do
        let empty_metadata = "0x6d65746109" :: HexString
            wrong_version = "0x6d657461ff" :: HexString
            wrong_magic = "0x6d64746110" :: HexString

        it "succeeds when the magic number matches" $
            let magic = decode empty_metadata
             in magic `shouldBe` Right MagicNumber

        it "fails when the magic number mismatches" $
            let error_str = decode wrong_magic :: Either String Metadata
             in error_str `shouldBe` Left "Failed reading: Bad magic number\nEmpty call stack\n"

        it "fails when version out of scope" $
            let error_str = decode wrong_version :: Either String Metadata
             in error_str `shouldBe` Left "Failed reading: wrong prefix during enum decoding\nEmpty call stack\n"

{-
    describe "Metadata V9" $ do
        it "succeeds decode from static hex" $
            let meta_v9 = [hexFrom|tests/meta/v9.hex|]
             in decode meta_v9 `shouldBe` Right (MetadataVersioned MagicNumber (V9 _))
-}

