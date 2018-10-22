{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Solidity.Test.CompilerSpec where

import           Language.Solidity.Compiler
import           Test.Hspec

spec :: Spec
spec = describe "solidity" $ do
    it "can compile empty contract" $ do
        compile (Sources [("A", "contract A {}")] [] True)
            `shouldBe` Right [("A", ("[]\n", "6080604052348015600f57600080fd5b50603580601d6000396000f3006080604052600080fd00a165627a7a72305820613b8f0a4aab50fea86c1e4943bcddad6d753778395309a4e055efcda614b0690029"))]

    it "can handle broken contract" $ do
        compile (Sources [("Fail", "contract Fail {")] [] True)
            `shouldBe` Left "Fail:1:16: Error: Function, variable, struct or modifier declaration expected.\ncontract Fail {\n               ^\n"

    it "can compile simple contract" $ do
        compile (Sources [("SimpleStorage", "contract SimpleStorage { uint256 public a; function set(uint256 _a) { a = _a; } }")] [] True)
            `shouldBe` Right [("SimpleStorage", ("[\n\t{\n\t\t\"constant\" : true,\n\t\t\"inputs\" : [],\n\t\t\"name\" : \"a\",\n\t\t\"outputs\" : \n\t\t[\n\t\t\t{\n\t\t\t\t\"name\" : \"\",\n\t\t\t\t\"type\" : \"uint256\"\n\t\t\t}\n\t\t],\n\t\t\"payable\" : false,\n\t\t\"stateMutability\" : \"view\",\n\t\t\"type\" : \"function\"\n\t},\n\t{\n\t\t\"constant\" : false,\n\t\t\"inputs\" : \n\t\t[\n\t\t\t{\n\t\t\t\t\"name\" : \"_a\",\n\t\t\t\t\"type\" : \"uint256\"\n\t\t\t}\n\t\t],\n\t\t\"name\" : \"set\",\n\t\t\"outputs\" : [],\n\t\t\"payable\" : false,\n\t\t\"stateMutability\" : \"nonpayable\",\n\t\t\"type\" : \"function\"\n\t}\n]\n", "608060405234801561001057600080fd5b5060dc8061001f6000396000f3006080604052600436106049576000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff1680630dbe671f14604e57806360fe47b1146076575b600080fd5b348015605957600080fd5b50606060a0565b6040518082815260200191505060405180910390f35b348015608157600080fd5b50609e6004803603810190808035906020019092919050505060a6565b005b60005481565b80600081905550505600a165627a7a7230582053764f3cc73b0960abb0d97899a8133bee5eb98c131978821f7add85f33d4f2e0029"))]
