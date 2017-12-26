module Network.Ethereum.Web3.Test.ERC20Spec where

import           Network.Ethereum.Web3.Test.Utils
import           Test.Hspec

spec :: Spec
spec = describe "interacting with an ERC20 contract" $ do
    it "should inject contract addresses" injectExportedEnvironmentVariables
    it "can perform a transfer" pending
