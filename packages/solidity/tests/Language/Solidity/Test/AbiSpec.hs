{-# LANGUAGE OverloadedStrings #-}
module Language.Solidity.Test.AbiSpec where


import           Data.Either           (isLeft)
import           Data.Text             (Text)
import           Language.Solidity.Abi
import           Test.Hspec


spec :: Spec
spec = parallel $ do
  describe "parseSolidityType" $
    describe "tuple type" $ do
        it "can parses a FunctionArg with tuple type" $ do
          let maa = FunctionArg  "makerAssetAmount" "uint256" Nothing
              ma = FunctionArg "makeAddress" "address" Nothing
              tupleFA = FunctionArg "order" "tuple" (Just [maa, ma])
              eRes = parseSolidityFunctionArgType tupleFA
          eRes `shouldBe` Right (SolidityTuple [SolidityUint 256, SolidityAddress])
        it "fails to parse a FunctionArg with invalid tuple" $ do
          let tupleFA = FunctionArg "order" "tuple" Nothing
              eRes = parseSolidityFunctionArgType tupleFA
          isLeft eRes `shouldBe` True
  describe "signature" $ do
    it "can generate signature for fillOrder" $ do
      let expected = "fillOrder((address,address,address,address,uint256,uint256,uint256,uint256,uint256,uint256,bytes,bytes),uint256,bytes)"
          sig = signature fillOrderDec
      sig `shouldBe` expected
    it "can generate signature for fillManyOrders" $ do
      let expected = "fillManyOrders((address,address,address,address,uint256,uint256,uint256,uint256,uint256,uint256,bytes,bytes)[],uint256,bytes)"
          sig = signature fillManyOrdersDec
      sig `shouldBe` expected
  describe "methodId" $ do
    it "can generate methodId for fillOrder" $ do
      let expected = "0xb4be83d5"
          mId = methodId fillOrderDec
      mId `shouldBe` expected
    it "can generate methodId for fillManyOrders" $ do
      let expected = "0xd52e8a68"
          mId = methodId fillManyOrdersDec
      mId `shouldBe` expected

orderTupleComponents :: [(Text, Text)]
orderTupleComponents =
  [ ("makerAddress", "address")
  , ("takerAddress", "address")
  , ("feeRecipientAddress", "address")
  , ("senderAddress", "address")
  , ("makerAssetAmount", "uint256")
  , ("takerAssetAmount", "uint256")
  , ("makerFee", "uint256")
  , ("takerFee", "uint256")
  , ("expirationTimeSeconds", "uint256")
  , ("salt", "uint256")
  , ("makerAssetData", "bytes")
  , ("takerAssetData", "bytes")
  ]

fillOrderDec :: Declaration
fillOrderDec = DFunction "fillOrder" False funInputs' funOutputs'
  where
    funInputs' =
      [ makeTupleFuncArg ("order", "tuple") orderTupleComponents
      , makeBasicFuncArg ("takerAssetFillAmount", "uint256")
      , makeBasicFuncArg ("signature", "bytes")
      ]
    funOutputs' = Nothing
    makeBasicFuncArg (n,t) =
      FunctionArg n t Nothing
    makeTupleFuncArg (n,t) cmps =
      FunctionArg n t (Just $ map makeBasicFuncArg cmps)

fillManyOrdersDec :: Declaration
fillManyOrdersDec = DFunction "fillManyOrders" False funInputs' funOutputs'
  where
    funInputs' =
      [ makeTupleFuncArg ("orders", "tuple[]") orderTupleComponents
      , makeBasicFuncArg ("takerAssetFillAmount", "uint256")
      , makeBasicFuncArg ("signature", "bytes")
      ]
    funOutputs' = Nothing
    makeBasicFuncArg (n,t) =
      FunctionArg n t Nothing
    makeTupleFuncArg (n,t) cmps =
      FunctionArg n t (Just $ map makeBasicFuncArg cmps)
