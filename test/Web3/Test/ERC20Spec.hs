module Web3.Test.ERC20Spec where

import           Test.Hspec
import           Web3.Test.Utils

spec :: Spec
spec = describe "interacting with an ERC20 contract" $ do
    it "should inject contract addresses" injectExportedEnvironmentVariables
    it "can perform a transfer" pending
