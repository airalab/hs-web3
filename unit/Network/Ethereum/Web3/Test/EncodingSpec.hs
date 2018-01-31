{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}

module Network.Ethereum.Web3.Test.EncodingSpec where

import qualified Data.ByteArray                         as BA
import           Data.ByteString                        (ByteString)
import qualified Data.ByteString.Base16                 as BS16
import           Data.Maybe                             (fromJust)
import           Data.Monoid
import           Data.Sized
import qualified Data.Text                              as T
import qualified Data.Text.Encoding                     as T
import qualified Data.Text.Lazy                         as TL
import           Data.Text.Lazy.Builder                 (toLazyText)
import           Generics.SOP                           (Generic, Rep)
import           Network.Ethereum.Web3                  hiding (convert)
import           Network.Ethereum.Web3.Encoding
import           Network.Ethereum.Web3.Encoding.Generic
import           Network.Ethereum.Web3.Encoding.Int
import           Network.Ethereum.Web3.Encoding.Vector
import           Test.Hspec


spec :: Spec
spec = do
  intNTest
  bytesDTest
  bytesNTest
  vectorTest
  dynamicArraysTest
  tuplesTest

intNTest :: Spec
intNTest =
    describe "uint tests" $ do

      it "can encode integer" $ do
         let decoded = (10 :: Integer)
             encoded = "000000000000000000000000000000000000000000000000000000000000000a"
         roundTrip decoded encoded

      it "can encode int16" $ do
         let decoded = fromJust . intNFromInteger $ -1 :: IntN 16
             encoded = "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
         roundTrip decoded encoded

      it "can encode larger uint256" $ do
         let decoded = fromJust . uIntNFromInteger $ (2 ^ 255) - 1 :: UIntN 256
             encoded = "7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
         roundTrip decoded encoded

      it "can fail to encode larger int248" $ do
         let muint = uIntNFromInteger $ (2 ^ 255) - 1 :: Maybe (UIntN 248)
         muint `shouldBe` Nothing

      it "can fail to encode larger negative int248" $ do
         let mint = intNFromInteger $ - (2 ^ 255 + 1) :: Maybe (IntN 248)
         mint `shouldBe` Nothing


bytesDTest :: Spec
bytesDTest = do
    describe "bytesD tests" $ do

      it "can encode short bytesD" $ do
         let decoded = Bytes . bytesDecode $ "c3a40000c3a4"
         let encoded = "0000000000000000000000000000000000000000000000000000000000000006"
                    <> "c3a40000c3a40000000000000000000000000000000000000000000000000000"
         roundTrip decoded encoded

      it "can encode long bytesD" $ do
         let decoded = Bytes . bytesDecode $
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

dynamicArraysTest :: Spec
dynamicArraysTest = do
    describe "dynamically sized array tests" $ do

      it "can encode dynamically sized lists of bools" $ do
         let decoded = [True, True, False]
             encoded = "0000000000000000000000000000000000000000000000000000000000000003"
                    <> "0000000000000000000000000000000000000000000000000000000000000001"
                    <> "0000000000000000000000000000000000000000000000000000000000000001"
                    <> "0000000000000000000000000000000000000000000000000000000000000000"
         roundTrip decoded encoded

tuplesTest :: Spec
tuplesTest =
  describe "tuples test" $ do

    it "can encode 2-tuples with both static args" $ do
      let decoded = (True, False)
          encoded = "0000000000000000000000000000000000000000000000000000000000000001"
                 <> "0000000000000000000000000000000000000000000000000000000000000000"
      roundTripGeneric decoded encoded

    it "can encode 1-tuples with dynamic arg" $ do
      let decoded = Singleton [True, False]
          encoded = "0000000000000000000000000000000000000000000000000000000000000020"
                 <> "0000000000000000000000000000000000000000000000000000000000000002"
                 <> "0000000000000000000000000000000000000000000000000000000000000001"
                 <> "0000000000000000000000000000000000000000000000000000000000000000"
      roundTripGeneric decoded encoded

    it "can encode 4-tuples with a mix of args -- (UInt, String, Boolean, Array Int)" $ do
      let decoded = (1 :: Integer, "dave" :: T.Text, True, [1,2,3] :: [Integer])
          encoded = "0000000000000000000000000000000000000000000000000000000000000001"
                 <> "0000000000000000000000000000000000000000000000000000000000000080"
                 <> "0000000000000000000000000000000000000000000000000000000000000001"
                 <> "00000000000000000000000000000000000000000000000000000000000000c0"
                 <> "0000000000000000000000000000000000000000000000000000000000000004"
                 <> "6461766500000000000000000000000000000000000000000000000000000000"
                 <> "0000000000000000000000000000000000000000000000000000000000000003"
                 <> "0000000000000000000000000000000000000000000000000000000000000001"
                 <> "0000000000000000000000000000000000000000000000000000000000000002"
                 <> "0000000000000000000000000000000000000000000000000000000000000003"
      roundTripGeneric decoded encoded

    it "can do something really complicated" $ do
      let uint = fromJust $ uIntNFromInteger 1 :: UIntN 256
          int = fromJust $ intNFromInteger (-1) :: IntN 256
          bool = True
          int224 = fromJust $ intNFromInteger 221 :: IntN 224
          bools = True :< False :< NilL :: Vector 2 Bool
          ints = [ fromJust $ intNFromInteger 1
                 , fromJust $ intNFromInteger (-1)
                 , fromJust $ intNFromInteger 3
                 ] :: [IntN 256]
          string = "hello" :: T.Text
          bytes16 =  BytesN $ bytesDecode "12345678123456781234567812345678" :: BytesN 16
          elem = BytesN $ bytesDecode "1234" :: BytesN 2
          bytes2s = [ elem :< elem :< elem :< elem :< NilL
                    , elem :< elem :< elem :< elem :< NilL
                    ] :: [Vector 4 (BytesN 2)]

          decoded = (uint, int, bool, int224, bools, ints, string, bytes16, bytes2s)

          encoded = "0000000000000000000000000000000000000000000000000000000000000001"
                 <> "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
                 <> "0000000000000000000000000000000000000000000000000000000000000001"
                 <> "00000000000000000000000000000000000000000000000000000000000000dd"
                 <> "0000000000000000000000000000000000000000000000000000000000000001"
                 <> "0000000000000000000000000000000000000000000000000000000000000000"
                 <> "0000000000000000000000000000000000000000000000000000000000000140"
                 <> "00000000000000000000000000000000000000000000000000000000000001c0"
                 <> "1234567812345678123456781234567800000000000000000000000000000000"
                 <> "0000000000000000000000000000000000000000000000000000000000000200"
                 <> "0000000000000000000000000000000000000000000000000000000000000003"
                 <> "0000000000000000000000000000000000000000000000000000000000000001"
                 <> "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
                 <> "0000000000000000000000000000000000000000000000000000000000000003"
                 <> "0000000000000000000000000000000000000000000000000000000000000005"
                 <> "68656c6c6f000000000000000000000000000000000000000000000000000000"
                 <> "0000000000000000000000000000000000000000000000000000000000000002"
                 <> "1234000000000000000000000000000000000000000000000000000000000000"
                 <> "1234000000000000000000000000000000000000000000000000000000000000"
                 <> "1234000000000000000000000000000000000000000000000000000000000000"
                 <> "1234000000000000000000000000000000000000000000000000000000000000"
                 <> "1234000000000000000000000000000000000000000000000000000000000000"
                 <> "1234000000000000000000000000000000000000000000000000000000000000"
                 <> "1234000000000000000000000000000000000000000000000000000000000000"
                 <> "1234000000000000000000000000000000000000000000000000000000000000"

      roundTripGeneric decoded encoded

-- utils
bytesDecode :: T.Text -> BA.Bytes
bytesDecode = BA.convert . fst . BS16.decode . T.encodeUtf8

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
