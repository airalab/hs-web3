{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

-- |
-- Module      :  Codec.Scale.Test.MetadataSpec
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
import           Data.Aeson                            (eitherDecodeFileStrict)
import           Data.ByteArray.HexString              (HexString, hexFrom)
import           Network.Polkadot.Metadata             (Metadata)
import           Network.Polkadot.Metadata.MagicNumber (MagicNumber (..))
import           Network.Polkadot.Metadata.Type        (sanitize)
import           Test.Hspec

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
             in error_str `shouldBe` Left "Failed reading: wrong prefix during enum decoding\nEmpty call stack\n"

    describe "Sanitize alias" $ do
        it "replaces all occurrences for types" $
            sanitize "(String,Address,MasterString,String)"
                `shouldBe` "(Text,Address,MasterString,Text)"

        it "replaces actual types, but leaves struct names" $
            sanitize "{\"system\":\"String\",\"versionString\":\"String\"}"
                `shouldBe` "{\"system\":\"Text\",\"versionString\":\"Text\"}"

        it "handles the preceding correctly" $
            sanitize "String String (String,[String;32],String)\"String<String>"
                `shouldBe` "Text Text (Text,[Text;32],Text)\"Text<Text>"

        it "handles emdedded Vec/Tuples" $
            sanitize "


{-
    describe "Metadata V9" $ do
        it "succeeds decode from hex and json" $ do
            let hex_v9 = decode [hexFrom|tests/meta/v9.hex|] :: Either String Metadata
            json_v9 <- eitherDecodeFileStrict "tests/meta/v9.json"
            hex_v9 `shouldBe` json_v9
-}
