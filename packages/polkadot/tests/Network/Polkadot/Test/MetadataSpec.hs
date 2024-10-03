{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

-- |
-- Module      :  Network.Polkadot.Test.MetadataSpec
-- Copyright   :  Aleksandr Krupenkin 2016-2024
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

import           Codec.Scale                           (decode, encode)
import           Data.Aeson                            (eitherDecodeFileStrict,
                                                        parseJSON, toJSON)
import           Data.ByteArray.HexString              (HexString)
import           Data.ByteArray.HexString.TH           (hexFrom)
import           Test.Hspec
import           Test.Hspec.Expectations.Json          (shouldBeJson)

import           Network.Polkadot.Metadata             (Metadata, metadataTypes)
import           Network.Polkadot.Metadata.MagicNumber (MagicNumber (..))

spec :: Spec
spec = parallel $ do
    describe "MagicNumber codec" $ do
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
             in error_str `shouldBe` Left "Failed reading: index out of enum constructors count: 241\nEmpty call stack\n"

    describe "Metadata V9" $ do
        it "succeeds decode from hex and json" $ do
            let (Right hex) = decode [hexFrom|tests/meta/v9.hex|] :: Either String Metadata
                (meta, _) = metadataTypes hex
            Right json <- eitherDecodeFileStrict "tests/meta/v9.json"
            toJSON meta `shouldBeJson` json

    describe "Metadata V10" $ do
        it "succeeds decode from hex and json" $ do
            let (Right hex) = decode [hexFrom|tests/meta/v10.hex|] :: Either String Metadata
                (meta, _) = metadataTypes hex
            Right json <- eitherDecodeFileStrict "tests/meta/v10.json"
            toJSON meta `shouldBeJson` json

{-
    describe "Metadata V12" $ do
        it "succeeds decode from hex and json" $ do
            let (Right hex) = decode [hexFrom|tests/meta/v12.hex|] :: Either String Metadata
                (meta, _) = metadataTypes hex
            Right json <- eitherDecodeFileStrict "tests/meta/v12.json"
            toJSON meta `shouldBeJson` json

    describe "Metadata V13" $ do
        it "succeeds decode from hex and json" $ do
            let (Right hex) = decode [hexFrom|tests/meta/v13.hex|] :: Either String Metadata
                (meta, _) = metadataTypes hex
            Right json <- eitherDecodeFileStrict "tests/meta/v13.json"
            toJSON meta `shouldBeJson` json
-}
