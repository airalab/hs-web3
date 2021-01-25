{-# LANGUAGE OverloadedStrings #-}
module Language.Solidity.Test.AbiSpec where


import           Data.Either           (isLeft)
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
  describe "signature" $
    it "can generate signature for fillOrder" $ do
      let fillOrderDec = buildFillOrderDec
          expected = "fillOrder((address,address,address,address,uint256,uint256,uint256,uint256,uint256,uint256,bytes,bytes),uint256,bytes)"
          sig = signature fillOrderDec
      sig `shouldBe` expected
  describe "methodId" $
    it "can generate methodId for fillOrder" $ do
      let fillOrderDec = buildFillOrderDec
          expected = "0xb4be83d5"
          mId = methodId fillOrderDec
      mId `shouldBe` expected

buildFillOrderDec :: Declaration
buildFillOrderDec = DFunction "fillOrder" False funInputs' funOutputs'
  where
    funInputs' =
      [ makeTupleFuncArg ("order", "tuple") tupleComponents
      , makeBasicFuncArg ("takerAssetFillAmount", "uint256")
      , makeBasicFuncArg ("signature", "bytes")
      ]
    tupleComponents =
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
      , ("makerAssetData",   "bytes")
      , ("takerAssetData",   "bytes")
      ]
    funOutputs' = Nothing
    makeBasicFuncArg (n,t) =
      FunctionArg n t Nothing
    makeTupleFuncArg (n,t) cmps =
      FunctionArg n t (Just $ map makeBasicFuncArg cmps)
