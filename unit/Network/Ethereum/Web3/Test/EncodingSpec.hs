{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Network.Ethereum.Web3.Test.EncodingSpec where

import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import           Generics.SOP               (Generic, Rep)
import           Test.Hspec

import           Data.HexString             (hexString)
import           Data.Solidity.Abi          (AbiGet, AbiPut, GenericAbiGet,
                                             GenericAbiPut)
import           Data.Solidity.Abi.Codec    (decode, decode', encode, encode')
import           Data.Solidity.Prim         (Address, Bytes, BytesN, IntN,
                                             ListN, Singleton (..), UIntN)
import           Data.Solidity.Prim.Address (fromHexString, toHexString)

spec :: Spec
spec = do
  intNTest
  bytesTest
  bytesNTest
  vectorTest
  dynamicArraysTest
  tuplesTest
  addressTest

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

      it "can encode empty bytes" $ do
         let decoded :: Bytes
             decoded = "0x"
             encoded = "0x0000000000000000000000000000000000000000000000000000000000000000"
          in roundTrip decoded encoded

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

      it "can pad shorter hex-literals" $ do
         let literal = "0xc3a4" :: BytesN 4
             expected = "0xc3a40000" :: BytesN 4
         literal `shouldBe` expected

      it "can pad shorter string-literals" $ do
         let literal = "hello" :: BytesN 8
             expected = "0x68656c6c6f000000" :: BytesN 8
         literal `shouldBe` expected

{-
      it "fails on too long literals" $ do
         let literal = "hello" :: BytesN 4
         evaluate literal `shouldThrow` errorCall "Invalid Size"
-}

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
          elem1 = "0x1234" :: BytesN 2
          bytes2s = [ [elem1, elem1, elem1, elem1]
                    , [elem1, elem1, elem1, elem1]
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

addressTest :: Spec
addressTest =
  describe "address test" $ do
    it "can abi encode address" $ do
      let decoded = "0x4af013AfBAdb22D8A88c92D68Fc96B033b9Ebb8a" :: Address
          encoded = "0x0000000000000000000000004af013AfBAdb22D8A88c92D68Fc96B033b9Ebb8a"
       in roundTrip decoded encoded

    it "can decode address from/to bytestring" $ do
      fromHexString "0x4af013AfBAdb22D8A88c92D68Fc96B033b9Ebb8a"
        `shouldBe` Right ("0x4af013AfBAdb22D8A88c92D68Fc96B033b9Ebb8a" :: Address)

      toHexString "0x4af013AfBAdb22D8A88c92D68Fc96B033b9Ebb8a"
        `shouldBe` "0x4af013afbadb22d8a88c92d68fc96b033b9ebb8a"

    it "fails for invalid address length" $ do
      (fromHexString =<< hexString "0x0")
        `shouldBe` Left "base16: input: invalid length"

      (fromHexString =<< hexString "0x4af013AfBAdb22D8A88c92D68Fc96B033b9Ebb8a00")
        `shouldBe` Left "Incorrect address length: 21"

-- | Run encoded/decoded comaration
roundTrip :: ( Show a
             , Eq a
             , AbiPut a
             , AbiGet a
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
                    , GenericAbiPut rep
                    , GenericAbiGet rep
                    )
                 => a
                 -> Bytes
                 -> IO ()
roundTripGeneric decoded encoded = do
  encoded `shouldBe` encode' decoded
  decode' encoded `shouldBe` Right decoded
