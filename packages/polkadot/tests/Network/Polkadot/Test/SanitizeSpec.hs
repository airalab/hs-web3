{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Codec.Scale.Test.SanitizeSpec
-- Copyright   :  Aleksandr Krupenkin 2016-2020
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- Polkadot type name sanitize tests ported from:
-- https://github.com/polkadot-js/api/blob/v2.3.1/packages/types/src/create/sanitize.spec.ts
--

module Network.Polkadot.Test.SanitizeSpec where

import           Codec.Scale                           (decode)
import           Data.Aeson                            (eitherDecodeFileStrict)
import           Data.ByteArray.HexString              (HexString, hexFrom)
import           Network.Polkadot.Metadata             (Metadata)
import           Network.Polkadot.Metadata.MagicNumber (MagicNumber (..))
import           Network.Polkadot.Metadata.Type        (sanitize)
import           Test.Hspec

spec :: Spec
spec = parallel $ do
    describe "alias" $ do
        let exec = alias ["String"] "Text"

        it "replaces all occurrences for types" $
            exec "(String,Address,MasterString,String)"
                `shouldBe` "(Text,Address,MasterString,Text)"

        it "replaces actual types, but leaves struct names" $
            exec "{\"system\":\"String\",\"versionString\":\"String\"}"
                `shouldBe` "{\"system\":\"Text\",\"versionString\":\"Text\"}"

        it "handles the preceding correctly" $
            exec "String String (String,[String;32],String)\"String<String>"
                `shouldBe` "Text Text (Text,[Text;32],Text)\"Text<Text>"

        it "handles emdedded Vec/Tuples" $ do
            let ann = alias ["Announcement"] "ProxyAnnouncement"
            ann "(Vec<Announcement>,BalanceOf)"
                `shouldBe` "(Vec<ProxyAnnouncement>,BalanceOf)"

    describe "removeColons" $ do
        it "removes preceding ::Text -> Text" $
            removeColons "::Text" `shouldBe` "Text"

        it "removes middle voting::TallyType -> TallyType" $
            removeColons "voting::TallyType" `shouldBe` "TallyType"

        it "removes on embedded values (one)" $
            removeColons "(T::AccountId, SpanIndex)" `shouldBe` "(AccountId, SpanIndex)"

        it "removes on embedded values (all)" $
            removeColons "(T::AccountId, slashing::SpanIndex)"
                `shouldBe` "(AccountId, SpanIndex)"

        it "keeps with allowNamespaces" $
            removeColons "::slashing::SpanIndex" `shouldBe` "slashing::SpanIndex"
