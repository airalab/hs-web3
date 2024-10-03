{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

-- |
-- Module      :  Network.Polkadot.Test.ExtrinsicSpec
-- Copyright   :  Aleksandr Krupenkin 2016-2024
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
--

module Network.Polkadot.Test.ExtrinsicSpec where

import           Codec.Scale                    (Compact (..), decode)
import           Data.ByteArray.HexString.TH    (hex)
import           Test.Hspec

import           Network.Polkadot.Extrinsic.Era (Era (..), new_mortal_compact)

spec :: Spec
spec = parallel $ do
    describe "Era" $ do
        it "decodes an Extrinsic Era with immortal" $
            decode [hex|0x00|] `shouldBe` Right ImmortalEra

        it "creates from an actual valid era" $
            new_mortal_compact 0xc503 `shouldBe` MortalEra 64 60

        it "creates for an actual era (2)" $
            new_mortal_compact 0x8502 `shouldBe` MortalEra 64 40

        it "creates form an actual era (3)" $
            new_mortal_compact 0x6502 `shouldBe` MortalEra 64 38

        it "creates from a actual 100 block hash count" $
            new_mortal_compact 0xd607 `shouldBe` MortalEra 128 125

        it "creates from a actual 2400 block hash count" $
            new_mortal_compact 0x9be3 `shouldBe` MortalEra 4096 3641
