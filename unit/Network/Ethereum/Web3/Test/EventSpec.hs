{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}

module Network.Ethereum.Web3.Test.EventSpec where

import qualified GHC.Generics as GHC
import Generics.SOP
import Data.Tagged
import Network.Ethereum.Web3
import Network.Ethereum.Web3.Types
import Network.Ethereum.Web3.Encoding.Event
import Network.Ethereum.Web3.Encoding
import Network.Ethereum.Web3.Encoding.Generic
import Test.Hspec

spec :: Spec
spec = eventTest


eventTest :: Spec
eventTest =
    describe "event tests" $ do

      it "can decode simple storage" $ do
         let change = Change { changeLogIndex = "0x2"
                             , changeTransactionIndex = "0x2"
                             , changeTransactionHash = "0xe8cac6af0ceb3cecbcb2a5639361fc9811b1aa753672cf7c7e8b528df53e0e94"
                             , changeBlockHash = "0x0c7e1701858232ac210e3bcc8ab3b33cc6b08025692b22abb39059dc41f6a76e"
                             , changeBlockNumber = 0
                             , changeAddress = "0x617e5941507aab5d2d8bcb56cb8c6ce2eeb16b21"
                             , changeData = "0x000000000000000000000000000000000000000000000000000000000000000a"
                             , changeTopics = ["0xa32bc18230dd172221ac5c4821a5f1f1a831f27b1396d244cdd891c58f132435"]
                             }
         decodeEvent change `shouldBe` Just (NewCount 10)

      it "can decode erc20" $ do
         let ercchange = Change { changeLogIndex = "0x2"
                                , changeTransactionIndex = "0x2"
                                , changeTransactionHash = "0xe8cac6af0ceb3cecbcb2a5639361fc9811b1aa753672cf7c7e8b528df53e0e94"
                                , changeBlockHash = "0x0c7e1701858232ac210e3bcc8ab3b33cc6b08025692b22abb39059dc41f6a76e"
                                , changeBlockNumber = 0
                                , changeAddress = "0x617e5941507aab5d2d8bcb56cb8c6ce2eeb16b21"
                                , changeData = "0x000000000000000000000000000000000000000000000000000000000000000a"
                                , changeTopics = [ "0xb32bc18230dd172221ac5c4821a5f1f1a831f27b1396d244cdd891c58f132435"
                                                 , "0x0000000000000000000000000000000000000000000000000000000000000001"
                                                 , "0x0000000000000000000000000000000000000000000000000000000000000002"
                                                 ]
                                }
         decodeEvent ercchange `shouldBe` Just (Transfer "0x0000000000000000000000000000000000000001" 10 "0x0000000000000000000000000000000000000002")

-- SimpleStorage Event Types

data NewCount = NewCount Integer deriving (Eq, Show, GHC.Generic)
instance Generic NewCount

data NewCountIndexed = NewCountIndexed  deriving (Eq, Show, GHC.Generic)
instance Generic NewCountIndexed

data NewCountNonIndexed = NewCountNonIndexed (Tagged 1 Integer) deriving (Eq, Show, GHC.Generic)
instance Generic NewCountNonIndexed

instance IndexedEvent NewCountIndexed NewCountNonIndexed NewCount where
  isAnonymous = const False

-- WeirdERC20 Event Types (Transfer type wrong order for testing purposes)
data Transfer = Transfer Address Integer Address deriving (Eq, Show, GHC.Generic)
instance Generic Transfer

data TransferIndexed = TransferIndexed (Tagged 1 Address) (Tagged 3 Address) deriving (Eq, Show, GHC.Generic)
instance Generic TransferIndexed

data TransferNonIndexed = TransferNonIndexed (Tagged 2 Integer) deriving (Eq, Show, GHC.Generic)
instance Generic TransferNonIndexed

instance IndexedEvent TransferIndexed TransferNonIndexed Transfer where
  isAnonymous = const False


