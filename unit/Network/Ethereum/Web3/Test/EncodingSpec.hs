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
import Network.Ethereum.Web3.Encoding.Vector
import Network.Ethereum.Web3.Encoding.Generic
import Data.Monoid
import Data.Sized
import Network.Ethereum.Web3 hiding (convert)
import Test.Hspec


spec :: Spec
spec = do
  bytesDTest
  bytesNTest
  vectorTest


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

vectorTest :: Spec
vectorTest =
    describe "statically sized array tests" $ do

      it "can encode statically sized vectors of addresses" $ do
         let decoded = False :< True :< NilL :: Vector 2 Bool
             encoded = "0000000000000000000000000000000000000000000000000000000000000000"
                    <> "0000000000000000000000000000000000000000000000000000000000000001"
         roundTrip decoded encoded

      it "can encode statically sized vectors of statically sized bytes"$  do
         let elem1 = BytesN . bytesDecode $ "cf" :: BytesN 1
             elem2 = BytesN . bytesDecode $ "68" :: BytesN 1
             elem3 = BytesN . bytesDecode $ "4d" :: BytesN 1
             elem4 = BytesN . bytesDecode $ "fb" :: BytesN 1
             decoded = elem1 :< elem2 :< elem3 :< elem4 :< NilL :: Vector 4 (BytesN 1)
             encoded = "cf00000000000000000000000000000000000000000000000000000000000000"
                    <> "6800000000000000000000000000000000000000000000000000000000000000"
                    <> "4d00000000000000000000000000000000000000000000000000000000000000"
                    <> "fb00000000000000000000000000000000000000000000000000000000000000"
         roundTrip decoded encoded

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
