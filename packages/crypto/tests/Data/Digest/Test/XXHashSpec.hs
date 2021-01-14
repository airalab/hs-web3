{-# LANGUAGE OverloadedStrings #-}
module Data.Digest.Test.XXHashSpec where

import           Data.ByteArray           (convert)
import           Test.Hspec

import           Data.ByteArray.HexString (HexString)
import           Data.Digest.XXHash       (xxhash)

spec :: Spec
spec = parallel $ do
    describe "Variable lenght Twox" $ do
        it "64-bit output (Twox64)" $ do
            let hash = convert (xxhash 32 "abc") :: HexString
            hash `shouldBe` "0x990977adf52cbc44"

            let hash' = convert (xxhash 64 "abc") :: HexString
            hash' `shouldBe` "0x990977adf52cbc44"

        it "128-bit output (Twox128)" $ do
            let hash = convert (xxhash 65 "abc") :: HexString
            hash `shouldBe` "0x990977adf52cbc440889329981caa9be"

            let hash' = convert (xxhash 128 "abc") :: HexString
            hash' `shouldBe` "0x990977adf52cbc440889329981caa9be"

        it "256-bit output (Twox256)" $ do
            let hash = convert (xxhash 255 "abc") :: HexString
            hash `shouldBe` "0x990977adf52cbc440889329981caa9bef7da5770b2b8a05303b75d95360dd62b"

            let hash' = convert (xxhash 256 "abc") :: HexString
            hash' `shouldBe` "0x990977adf52cbc440889329981caa9bef7da5770b2b8a05303b75d95360dd62b"
