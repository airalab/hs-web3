{-# LANGUAGE FlexibleContexts #-}

module Network.Ethereum.Web3.Test.EncodingSpec where

import qualified Data.ByteString.Base16 as BS16
import Data.ByteString (ByteString)
import Data.ByteArray (Bytes, convert)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as T
import Data.Text.Lazy.Builder (toLazyText)
import Generics.SOP (Generic, Rep)

import Network.Ethereum.Web3.Encoding
import Network.Ethereum.Web3.Encoding.Generic
import Data.Monoid
import Network.Ethereum.Web3 hiding (convert)
import Test.Hspec


spec :: Spec
spec = do
  bytesDTest
  bytesNTest


bytesDTest :: Spec
bytesDTest = do
    describe "bytesD tests" $ do

      it "can encode short bytesD" $ do
         let decoded = BytesD . bytesDecode $ "c3a40000c3a4"
         let encoded = "0000000000000000000000000000000000000000000000000000000000000006"
                    <> "c3a40000c3a40000000000000000000000000000000000000000000000000000"
         roundTrip decoded encoded

      it "can encode long bytesD" $ do
         let decoded = BytesD . bytesDecode $
                            "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
                         <> "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
                         <> "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
                         <> "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
                         <> "fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff1"
         let encoded  =     "000000000000000000000000000000000000000000000000000000000000009f"
                         <> "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
                         <> "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
                         <> "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
                         <> "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
                         <> "fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff100"
         roundTrip decoded encoded

bytesNTest :: Spec
bytesNTest =
    describe "byteN tests" $ do

      it "can encode Bytes1" $ do
         let decoded = BytesN . bytesDecode $ "cf" :: BytesN 1
             encoded = "cf00000000000000000000000000000000000000000000000000000000000000"
         roundTrip decoded encoded

      it "can encode Bytes12" $ do
         let decoded = BytesN . bytesDecode $ "6761766f66796f726b000000" :: BytesN 12
             encoded = "6761766f66796f726b0000000000000000000000000000000000000000000000"
         roundTrip decoded encoded

-- utils
bytesDecode :: T.Text -> Bytes
bytesDecode = convert . fst . BS16.decode . T.encodeUtf8

roundTrip :: ( Show a
             , Eq a
             , ABIEncode a
             , ABIDecode a
             )
          => a
          -> T.Text
          -> IO ()
roundTrip decoded encoded = do
  encoded `shouldBe` ( TL.toStrict . toLazyText . toDataBuilder $ decoded)
  fromData encoded `shouldBe` Just decoded

roundTripGeneric :: ( Show a
                    , Eq a
                    , Generic a
                    , GenericABIEncode (Rep a)
                    , GenericABIDecode (Rep a)
                    )
                 => a
                 -> T.Text
                 -> IO ()
roundTripGeneric decoded encoded = do
  encoded `shouldBe` (TL.toStrict . toLazyText . genericABIEncode $ decoded)
  genericFromData encoded `shouldBe` Just decoded
