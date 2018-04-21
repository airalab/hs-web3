{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Network.Ethereum.Web3.Test.EncodingSpec where

import           Data.Monoid                         ((<>))
import           Data.Text                           (Text)
import           Generics.SOP                        (Generic, Rep)
import           Test.Hspec

import           Network.Ethereum.ABI.Class          (ABIGet, ABIPut,
                                                      GenericABIGet,
                                                      GenericABIPut)
import           Network.Ethereum.ABI.Codec          (decode, decode', encode,
                                                      encode')
import           Network.Ethereum.ABI.Prim.Bool      ()
import           Network.Ethereum.ABI.Prim.Bytes     (Bytes, BytesN)
import           Network.Ethereum.ABI.Prim.Int       (IntN, UIntN)
import           Network.Ethereum.ABI.Prim.List      (ListN)
import           Network.Ethereum.ABI.Prim.Singleton (Singleton (..))
import           Network.Ethereum.ABI.Prim.String    ()

spec :: Spec
spec = do
  intNTest
  bytesTest
  bytesNTest
  vectorTest
  dynamicArraysTest
  tuplesTest

intNTest :: Spec
intNTest =
    describe "uint tests" $ do

      it "can encode int16" $
         let decoded = (-1) :: IntN 16
             encoded = "0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
          in roundTrip decoded encoded

      it "can encode larger uint256" $
         let decoded = (2 ^ 255) - 1 :: UIntN 256
             encoded = "0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
          in roundTrip decoded encoded

bytesTest :: Spec
bytesTest = do
    describe "bytes tests" $ do

      it "can encode short bytes" $ do
         let decoded :: Bytes
             decoded = "0xc3a40000c3a4"
             encoded = "0x0000000000000000000000000000000000000000000000000000000000000006"
                    <> "0xc3a40000c3a40000000000000000000000000000000000000000000000000000"
          in roundTrip decoded encoded

      it "can encode long bytes" $ do
         let decoded :: Bytes
             decoded = "0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
                    <> "0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
                    <> "0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
                    <> "0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
                    <> "0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff1"
             encoded = "0x000000000000000000000000000000000000000000000000000000000000009f"
                    <> "0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
                    <> "0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
                    <> "0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
                    <> "0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
                    <> "0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff100"
          in roundTrip decoded encoded

bytesNTest :: Spec
bytesNTest =
    describe "sized bytes tests" $ do

      it "can encode Bytes1" $ do
         let decoded = "0xcf" :: BytesN 1
             encoded = "0xcf00000000000000000000000000000000000000000000000000000000000000"
         roundTrip decoded encoded

      it "can encode Bytes12" $ do
         let decoded = "0x6761766f66796f726b000000" :: BytesN 12
             encoded = "0x6761766f66796f726b0000000000000000000000000000000000000000000000"
         roundTrip decoded encoded

vectorTest :: Spec
vectorTest =
    describe "statically sized array tests" $ do

      it "can encode statically sized vectors of addresses" $ do
         let decoded = [False, True] :: ListN 2 Bool
             encoded = "0x0000000000000000000000000000000000000000000000000000000000000000"
                    <> "0x0000000000000000000000000000000000000000000000000000000000000001"
          in roundTrip decoded encoded

      it "can encode statically sized vectors of statically sized bytes"$  do
         let elem1 = "0xcf"
             elem2 = "0x68"
             elem3 = "0x4d"
             elem4 = "0xfb"
             decoded = [elem1, elem2, elem3, elem4] :: ListN 4 (BytesN 1)
             encoded = "0xcf00000000000000000000000000000000000000000000000000000000000000"
                    <> "0x6800000000000000000000000000000000000000000000000000000000000000"
                    <> "0x4d00000000000000000000000000000000000000000000000000000000000000"
                    <> "0xfb00000000000000000000000000000000000000000000000000000000000000"
          in roundTrip decoded encoded

dynamicArraysTest :: Spec
dynamicArraysTest = do
    describe "dynamically sized array tests" $ do

      it "can encode dynamically sized lists of bools" $ do
         let decoded = [True, True, False] :: [Bool]
             encoded = "0x0000000000000000000000000000000000000000000000000000000000000003"
                    <> "0x0000000000000000000000000000000000000000000000000000000000000001"
                    <> "0x0000000000000000000000000000000000000000000000000000000000000001"
                    <> "0x0000000000000000000000000000000000000000000000000000000000000000"
          in roundTrip decoded encoded

tuplesTest :: Spec
tuplesTest =
  describe "tuples test" $ do

    it "can encode 2-tuples with both static args" $ do
      let decoded = (True, False)
          encoded = "0x0000000000000000000000000000000000000000000000000000000000000001"
                 <> "0x0000000000000000000000000000000000000000000000000000000000000000"
       in roundTripGeneric decoded encoded

    it "can encode 1-tuples with dynamic arg" $ do
      let decoded = Singleton ([True, False] :: [Bool])
          encoded = "0x0000000000000000000000000000000000000000000000000000000000000020"
                 <> "0x0000000000000000000000000000000000000000000000000000000000000002"
                 <> "0x0000000000000000000000000000000000000000000000000000000000000001"
                 <> "0x0000000000000000000000000000000000000000000000000000000000000000"
       in roundTripGeneric decoded encoded

    it "can encode 4-tuples with a mix of args - (UInt32, String, Boolean, Array Int256)" $ do
      let decoded = (1 :: IntN 32, "dave" :: Text, True, [1, 2, 3] :: [IntN 256])
          encoded = "0x0000000000000000000000000000000000000000000000000000000000000001"
                 <> "0x0000000000000000000000000000000000000000000000000000000000000080"
                 <> "0x0000000000000000000000000000000000000000000000000000000000000001"
                 <> "0x00000000000000000000000000000000000000000000000000000000000000c0"
                 <> "0x0000000000000000000000000000000000000000000000000000000000000004"
                 <> "0x6461766500000000000000000000000000000000000000000000000000000000"
                 <> "0x0000000000000000000000000000000000000000000000000000000000000003"
                 <> "0x0000000000000000000000000000000000000000000000000000000000000001"
                 <> "0x0000000000000000000000000000000000000000000000000000000000000002"
                 <> "0x0000000000000000000000000000000000000000000000000000000000000003"
       in roundTripGeneric decoded encoded

    it "can do something really complicated" $ do
      let uint = 1 :: UIntN 256
          int = (-1) :: IntN 256
          bool = True
          int224 = 221 :: IntN 224
          bools = [True, False] :: ListN 2 Bool
          ints = [1, (-1), 3] :: [IntN 32]
          string = "hello" :: Text
          bytes16 =  "0x12345678123456781234567812345678" :: BytesN 16
          elem = "0x1234" :: BytesN 2
          bytes2s = [ [elem, elem, elem, elem]
                    , [elem, elem, elem, elem]
                    ] :: [ListN 4 (BytesN 2)]

          decoded = (uint, int, bool, int224, bools, ints, string, bytes16, bytes2s)

          encoded = "0x0000000000000000000000000000000000000000000000000000000000000001"
                 <> "0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
                 <> "0x0000000000000000000000000000000000000000000000000000000000000001"
                 <> "0x00000000000000000000000000000000000000000000000000000000000000dd"
                 <> "0x0000000000000000000000000000000000000000000000000000000000000001"
                 <> "0x0000000000000000000000000000000000000000000000000000000000000000"
                 <> "0x0000000000000000000000000000000000000000000000000000000000000140"
                 <> "0x00000000000000000000000000000000000000000000000000000000000001c0"
                 <> "0x1234567812345678123456781234567800000000000000000000000000000000"
                 <> "0x0000000000000000000000000000000000000000000000000000000000000200"
                 <> "0x0000000000000000000000000000000000000000000000000000000000000003"
                 <> "0x0000000000000000000000000000000000000000000000000000000000000001"
                 <> "0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
                 <> "0x0000000000000000000000000000000000000000000000000000000000000003"
                 <> "0x0000000000000000000000000000000000000000000000000000000000000005"
                 <> "0x68656c6c6f000000000000000000000000000000000000000000000000000000"
                 <> "0x0000000000000000000000000000000000000000000000000000000000000002"
                 <> "0x1234000000000000000000000000000000000000000000000000000000000000"
                 <> "0x1234000000000000000000000000000000000000000000000000000000000000"
                 <> "0x1234000000000000000000000000000000000000000000000000000000000000"
                 <> "0x1234000000000000000000000000000000000000000000000000000000000000"
                 <> "0x1234000000000000000000000000000000000000000000000000000000000000"
                 <> "0x1234000000000000000000000000000000000000000000000000000000000000"
                 <> "0x1234000000000000000000000000000000000000000000000000000000000000"
                 <> "0x1234000000000000000000000000000000000000000000000000000000000000"

       in roundTripGeneric decoded encoded

-- | Run encoded/decoded comaration
roundTrip :: ( Show a
             , Eq a
             , ABIPut a
             , ABIGet a
             )
          => a
          -> Bytes
          -> IO ()
roundTrip decoded encoded = do
  encoded `shouldBe` encode decoded
  decode encoded `shouldBe` Right decoded

-- | Run generic encoded/decoded comaration
roundTripGeneric :: ( Show a
                    , Eq a
                    , Generic a
                    , Rep a ~ rep
                    , GenericABIPut rep
                    , GenericABIGet rep
                    )
                 => a
                 -> Bytes
                 -> IO ()
roundTripGeneric decoded encoded = do
  encoded `shouldBe` encode' decoded
  decode' encoded `shouldBe` Right decoded
