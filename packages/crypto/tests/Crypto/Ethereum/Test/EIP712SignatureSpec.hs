{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}

module Crypto.Ethereum.Test.EIP712SignatureSpec (spec) where

import           Crypto.Ethereum.Eip712Signature
import           Crypto.Ethereum.Utils           (keccak256)
import           Data.Aeson                      (toJSON)
import qualified Data.Aeson                      as Aeson
import qualified Data.Aeson.KeyMap               as Aeson
import qualified Data.ByteArray.Encoding         as BAE
import           Data.ByteString                 (ByteString)
import           Data.Either                     (fromRight)
import           Test.Hspec

encodeTypeConcrete :: EIP712Types -> EIP712Name -> Either String ByteString
encodeTypeConcrete = encodeType

keccak256Concrete :: ByteString -> ByteString
keccak256Concrete = keccak256

hexEncode :: ByteString -> ByteString
hexEncode = BAE.convertToBase BAE.Base16

encodeDataConcrete :: EIP712Types -> EIP712Name -> Aeson.Object -> Either String ByteString
encodeDataConcrete = encodeData

hashStructConcrete :: EIP712Types -> EIP712Name -> Aeson.Object -> Either String ByteString
hashStructConcrete = hashStruct

mailTypes :: EIP712Types
mailTypes =
  [ EIP712Struct
      { eip712StructName = "Mail"
      , eip712StructFields =
          [ EIP712Field "from" (FieldTypeStruct "Person")
          , EIP712Field "to" (FieldTypeStruct "Person")
          , EIP712Field "contents" FieldTypeString
          ]
      }
  , EIP712Struct
      { eip712StructName = "Person"
      , eip712StructFields =
          [ EIP712Field "name" FieldTypeString
          , EIP712Field "wallet" FieldTypeAddress
          ]
      }
  , EIP712Struct
      { eip712StructName = "EIP712Domain"
      , eip712StructFields =
          [ EIP712Field "name" FieldTypeString
          , EIP712Field "version" FieldTypeString
          , EIP712Field "chainId" (FieldTypeUInt Si256)
          , EIP712Field "verifyingContract" FieldTypeAddress
          ]
      }
  ]

nestedArrayTypes :: EIP712Types
nestedArrayTypes =
  [ EIP712Struct
      { eip712StructName = "foo"
      , eip712StructFields =
          [ EIP712Field "a" (FieldTypeArray (FieldTypeArray (FieldTypeUInt Si256)))
          , EIP712Field "b" FieldTypeString
          ]
      }
  ]

safeTxType :: EIP712Struct
safeTxType =
  EIP712Struct
    { eip712StructName = "SafeTx"
    , eip712StructFields =
        [ EIP712Field "to" FieldTypeAddress
        , EIP712Field "value" (FieldTypeUInt Si256)
        , EIP712Field "data" FieldTypeBytes
        , EIP712Field "operation" (FieldTypeUInt Si8)
        , EIP712Field "safeTxGas" (FieldTypeUInt Si256)
        , EIP712Field "baseGas" (FieldTypeUInt Si256)
        , EIP712Field "gasPrice" (FieldTypeUInt Si256)
        , EIP712Field "gasToken" FieldTypeAddress
        , EIP712Field "refundReceiver" FieldTypeAddress
        , EIP712Field "nonce" (FieldTypeUInt Si256)
        ]
    }

expectedEncodedSafeTxType :: ByteString
expectedEncodedSafeTxType = "SafeTx(\
                                \address to,\
                                \uint256 value,\
                                \bytes data,\
                                \uint8 operation,\
                                \uint256 safeTxGas,\
                                \uint256 baseGas,\
                                \uint256 gasPrice,\
                                \address gasToken,\
                                \address refundReceiver,\
                                \uint256 nonce)"

safeDomainType :: EIP712Struct
safeDomainType =
            EIP712Struct
              { eip712StructName = "EIP712Domain"
              , eip712StructFields =
                  [ EIP712Field "chainId" (FieldTypeUInt Si256)
                  , EIP712Field "verifyingContract" FieldTypeAddress
                  ]
              }

safeDomain :: Aeson.KeyMap Aeson.Value
safeDomain =
            Aeson.fromList
              [("chainId", toJSON @Int 8453), ("verifyingContract", toJSON @String "0xCD2a3d9F938E13CD947Ec05AbC7FE734Df8DD826")]

expectedEncodedSafeDomain :: ByteString
expectedEncodedSafeDomain = "0000000000000000000000000000000000000000000000000000000000002105\
                            \000000000000000000000000cd2a3d9f938e13cd947ec05abc7fe734df8dd826"

safeMessage :: Aeson.KeyMap Aeson.Value
safeMessage =
    Aeson.fromList
      [ ("to",toJSON @String "0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" )
      , ("value", toJSON @Integer 0 )
      , ("data", toJSON @String "0x0b89085a01a3b67d2231c6a136f9c8eea75d7d479a83a127356f8540ee15af010c22b846886e98aeffc1f1166d4b3586")
      , ("operation", toJSON @Integer 0)
      , ("safeTxGas", toJSON @Integer 0)
      , ("baseGas", toJSON @Integer 0)
      , ("gasPrice", toJSON @Integer 0)
      , ("gasToken", toJSON @String "0x0000000000000000000000000000000000000000")
      , ("refundReceiver", toJSON @String "0x0000000000000000000000000000000000000000")
      , ("nonce", toJSON (37 :: Integer))
      ]

expectedEncodedSafeMessage :: ByteString
expectedEncodedSafeMessage = "000000000000000000000000aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\
                             \0000000000000000000000000000000000000000000000000000000000000000\
                             \059c15417e7e213ad30596d872d2e906e4feafd54fa0c9ac864b421ab1ba5adb\
                             \0000000000000000000000000000000000000000000000000000000000000000\
                             \0000000000000000000000000000000000000000000000000000000000000000\
                             \0000000000000000000000000000000000000000000000000000000000000000\
                             \0000000000000000000000000000000000000000000000000000000000000000\
                             \0000000000000000000000000000000000000000000000000000000000000000\
                             \0000000000000000000000000000000000000000000000000000000000000000\
                             \0000000000000000000000000000000000000000000000000000000000000025"

safeTxTypedData :: EIP712TypedData
safeTxTypedData = EIP712TypedData
  { typedDataTypes = [safeTxType, safeDomainType]
  , typedDataPrimaryType = "SafeTx"
  , typedDataDomain = safeDomain
  , typedDataMessage = safeMessage
  }


spec :: Spec
spec = do
  describe "encodeType" $ do
    it "simple types should be encoding properly" $ do
      let eip712structs = mailTypes
      encodeTypeConcrete eip712structs "Person" `shouldBe` Right "Person(string name,address wallet)"
      encodeTypeConcrete eip712structs "Mail"
        `shouldBe` Right "Mail(Person from,Person to,string contents)Person(string name,address wallet)"
      encodeTypeConcrete eip712structs "EIP712Domain"
        `shouldBe` Right "EIP712Domain(string name,string version,uint256 chainId,address verifyingContract)"
    it "should work for Safe transactions" $ do

      encodeTypeConcrete [safeDomainType] "EIP712Domain" `shouldBe` Right "EIP712Domain(uint256 chainId,address verifyingContract)"
      hexEncode . keccak256Concrete <$> encodeTypeConcrete [safeDomainType] "EIP712Domain"
        `shouldBe` Right "47e79534a245952e8b16893a336b85a3d9ea9fa8c573f3d803afb92a79469218"
      hexEncode <$> hashStruct [safeDomainType] "EIP712Domain" safeDomain
        `shouldBe` Right "b3a3e869527602e68d877d9edcc629823648c73a3b10ee1e23cf4ab81b599cf5"

    it "should work for safe transaction" $ encodeTypeConcrete [safeTxType] "SafeTx"
      `shouldBe` Right expectedEncodedSafeTxType

  describe "encodeData" $ do
    it "should encode simple data" $ do
      let eip712structs = mailTypes
      let typeName = "Person"
      let message =
            Aeson.fromList
              [("name", toJSON ("Cow" :: String)), ("wallet", toJSON ("0xCD2a3d9F938E13CD947Ec05AbC7FE734Df8DD826" :: String))]
      let encodedDataOrError = encodeDataConcrete eip712structs typeName message
      let expectedEncodedData = "8c1d2bd5348394761719da11ec67eedae9502d137e8940fee8ecd6f641ee1648\
                                \000000000000000000000000cd2a3d9f938e13cd947ec05abc7fe734df8dd826"
      hexEncode <$> encodedDataOrError `shouldBe` Right expectedEncodedData
      hashStructConcrete eip712structs "Person" message
        `shouldBe` Right (keccak256Concrete $ keccak256Concrete "Person(string name,address wallet)" <> fromRight undefined encodedDataOrError)

    it "should encode nested arrays" $ do
      let message =
            Aeson.fromList
              [ ("a", toJSON [[35 :: Int, 36], [37]])
              , ("b", toJSON ("hello" :: String))
              ]
      encodeTypeConcrete nestedArrayTypes "foo" `shouldBe` Right "foo(uint256[][] a,string b)"
      let expectedDataEncoded = "fa5ffe3a0504d850bc7c9eeda1cf960b596b73f4dc0272a6fa89dace08e32029\
                                \1c8aff950685c2ed4bc3174f3472287b56d9517b9c948127319a09a7a36deac8"
      hexEncode <$> encodeDataConcrete nestedArrayTypes "foo" message `shouldBe` Right expectedDataEncoded
    it "should encode Safe domain" $
        (hexEncode <$> encodeDataConcrete [safeDomainType] "EIP712Domain" safeDomain) `shouldBe` Right expectedEncodedSafeDomain

    it "should encode Safe transaction" $
        (hexEncode <$> encodeDataConcrete [safeTxType] "SafeTx" safeMessage) `shouldBe` Right expectedEncodedSafeMessage


  describe "typedDataSignHash" $ it "encode properly a safe transaction" $ do
    hexEncode <$> typedDataSignHash safeTxTypedData `shouldBe` Right "cd3b59061dd8a7060486fb14e75e2f066a19a6e93f6888dbf83c77fbfeb8874b"
