{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Solidity.Test.CompilerSpec where

import           Test.Hspec

#ifdef SOLIDITY_COMPILER

import           Language.Solidity.Compiler

spec :: Spec
spec = describe "solidity compiler" $ do
    it "can compile empty contract" $ do
        compile (Sources [("A", "contract A {}")] [] True)
            `shouldBe` Right [("A", ("[]\n", "6080604052348015600f57600080fd5b50603580601d6000396000f3fe6080604052600080fdfea165627a7a7230582055975a3cf5eb9a652c2154ede405cdb2137a09e47088fb89162c336da0b415c40029"))]

    it "can handle broken contract" $ do
        compile (Sources [("Fail", "contract Fail {")] [] True)
            `shouldBe` Left "Fail:1:16: Error: Function, variable, struct or modifier declaration expected.\ncontract Fail {\n               ^\n"

    it "can compile simple contract" $ do
        compile (Sources [("SimpleStorage", "contract SimpleStorage { uint256 public a; function set(uint256 _a) public { a = _a; } }")] [] True)
            `shouldBe` Right [("SimpleStorage", ("[\n\t{\n\t\t\"constant\" : true,\n\t\t\"inputs\" : [],\n\t\t\"name\" : \"a\",\n\t\t\"outputs\" : \n\t\t[\n\t\t\t{\n\t\t\t\t\"name\" : \"\",\n\t\t\t\t\"type\" : \"uint256\"\n\t\t\t}\n\t\t],\n\t\t\"payable\" : false,\n\t\t\"stateMutability\" : \"view\",\n\t\t\"type\" : \"function\"\n\t},\n\t{\n\t\t\"constant\" : false,\n\t\t\"inputs\" : \n\t\t[\n\t\t\t{\n\t\t\t\t\"name\" : \"_a\",\n\t\t\t\t\"type\" : \"uint256\"\n\t\t\t}\n\t\t],\n\t\t\"name\" : \"set\",\n\t\t\"outputs\" : [],\n\t\t\"payable\" : false,\n\t\t\"stateMutability\" : \"nonpayable\",\n\t\t\"type\" : \"function\"\n\t}\n]\n", "608060405234801561001057600080fd5b5060e38061001f6000396000f3fe6080604052600436106043576000357c0100000000000000000000000000000000000000000000000000000000900480630dbe671f14604857806360fe47b1146070575b600080fd5b348015605357600080fd5b50605a60a7565b6040518082815260200191505060405180910390f35b348015607b57600080fd5b5060a560048036036020811015609057600080fd5b810190808035906020019092919050505060ad565b005b60005481565b806000819055505056fea165627a7a7230582077f3d0f8c042c028ca18fb49065c0091b05c6b70dd5aee2b8a4388f7ecaa308f0029"))]

#else

spec :: Spec
spec = describe "solidity compiler is not enabled" $ return ()

#endif
