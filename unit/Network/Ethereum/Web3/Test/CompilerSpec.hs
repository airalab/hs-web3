{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Network.Ethereum.Web3.Test.CompilerSpec where

import           Language.Solidity.Compiler
import           Test.Hspec

spec :: Spec
spec = describe "solidity" $ do
    it "can compile empty contract" $ do
        compile (Sources [("A", "contract A {}")] [] True)
            `shouldBe` Right [("A", ("[]\n", "6080604052348015600f57600080fd5b50603580601d6000396000f3006080604052600080fd00a165627a7a723058206a08beaeb7393045de3d83f44db8e6d34b1b53821a3bc6647f641e5fbbdd679a0029"))]

    it "can handle broken contract" $ do
        compile (Sources [("Fail", "contract Fail {")] [] True)
            `shouldBe` Left "Fail:1:16: Error: Function, variable, struct or modifier declaration expected.\ncontract Fail {\n               ^\n"

    it "can compile simple contract" $ do
        compile (Sources [("SimpleStorage", "contract SimpleStorage { uint256 public a; function set(uint256 _a) { a = _a; } }")] [] True)
            `shouldBe` Right [("SimpleStorage", ("[\n\t{\n\t\t\"constant\" : true,\n\t\t\"inputs\" : [],\n\t\t\"name\" : \"a\",\n\t\t\"outputs\" : \n\t\t[\n\t\t\t{\n\t\t\t\t\"name\" : \"\",\n\t\t\t\t\"type\" : \"uint256\"\n\t\t\t}\n\t\t],\n\t\t\"payable\" : false,\n\t\t\"stateMutability\" : \"view\",\n\t\t\"type\" : \"function\"\n\t},\n\t{\n\t\t\"constant\" : false,\n\t\t\"inputs\" : \n\t\t[\n\t\t\t{\n\t\t\t\t\"name\" : \"_a\",\n\t\t\t\t\"type\" : \"uint256\"\n\t\t\t}\n\t\t],\n\t\t\"name\" : \"set\",\n\t\t\"outputs\" : [],\n\t\t\"payable\" : false,\n\t\t\"stateMutability\" : \"nonpayable\",\n\t\t\"type\" : \"function\"\n\t}\n]\n","608060405234801561001057600080fd5b5060dc8061001f6000396000f3006080604052600436106049576000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff1680630dbe671f14604e57806360fe47b1146076575b600080fd5b348015605957600080fd5b50606060a0565b6040518082815260200191505060405180910390f35b348015608157600080fd5b50609e6004803603810190808035906020019092919050505060a6565b005b60005481565b80600081905550505600a165627a7a72305820cfdbfa82d208da59706665bcf45431cdd8824420e1e3f68d74da826c1221bab10029"))]
