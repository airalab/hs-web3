{-# LANGUAGE OverloadedStrings #-}
module Data.Digest.Test.Blake2Spec where

import           Data.ByteArray           (convert)
import           Data.ByteArray.HexString (HexString)
import           Data.ByteString.Lazy     (toStrict)
import           Test.Hspec

import           Data.Digest.Blake2

spec :: Spec
spec = parallel $ do
    describe "Variable lenght Blake2" $ do
        it "64-bit output (Blake2b 64)" $
            let hash = convert (toStrict $ blake2_64 "abc") :: HexString
             in hash `shouldBe` "0xd8bb14d833d59559"

        it "128-bit output (Blake2b 128)" $
            let hash = convert (toStrict $ blake2_128 "abc") :: HexString
             in hash `shouldBe` "0xcf4ab791c62b8d2b2109c90275287816"

        it "256-bit output (Blake2b 256)" $
            let hash = convert (toStrict $ blake2_256 "abc") :: HexString
             in hash `shouldBe` "0xbddd813c634239723171ef3fee98579b94964e3bb1cb3e427262c8c068d52319"

        it "512-bit output (Blake2b 512)" $
            let hash = convert (toStrict $ blake2_512 "abc") :: HexString
             in hash `shouldBe` "0xba80a53f981c4d0d6a2797b69f12f6e94c212f14685ac4b74b12bb6fdbffa2d17d87c5392aab792dc252d5de4533cc9518d38aa8dbf1925ab92386edd4009923"
