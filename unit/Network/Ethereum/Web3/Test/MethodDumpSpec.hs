module Network.Ethereum.Web3.Test.MethodDumpSpec where

import           Control.Monad.IO.Class   (liftIO)
import           Data.Text                (unpack)
import           Network.Ethereum.Web3
import           Network.Ethereum.Web3.TH
import           Test.Hspec
import           Text.Printf


spec :: Spec
spec = describe "methodDump" $
    it "can dump an ABI" $  do
        let theApiDump = [abiFrom|data/ERC20.json|]
        theApiDump `shouldNotBe` ""
{-
    Right s <- runWeb3 $ do
        n <- name token
        s <- symbol token
        d <- decimals token
        return $ printf "Token %s with symbol %s and decimals %d"
                        (unpack n) (unpack s) d
    putStrLn s
  where token = "0x237D60A8b41aFD2a335305ed458B609D7667D789"
-}
