{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Network.Ethereum.Web3.Test.EventSpec where

import           Data.Tagged                (Tagged)
import           Generics.SOP               (Generic)
import qualified GHC.Generics               as GHC (Generic)
import           Test.Hspec                 (Spec, describe, it, shouldBe)

import           Data.Solidity.Abi          (AbiGet, AbiType (..))
import           Data.Solidity.Event        (IndexedEvent (..), decodeEvent)
import           Data.Solidity.Prim         (Address, UIntN)
import           Network.Ethereum.Api.Types (Change (..))


spec :: Spec
spec = eventTest

eventTest :: Spec
eventTest =
    describe "event tests" $ do

      it "can decode simple storage" $
         let change = Change { changeLogIndex = Just "0x2"
                             , changeTransactionIndex = Just "0x2"
                             , changeTransactionHash = Just "0xe8cac6af0ceb3cecbcb2a5639361fc9811b1aa753672cf7c7e8b528df53e0e94"
                             , changeBlockHash = Just "0x0c7e1701858232ac210e3bcc8ab3b33cc6b08025692b22abb39059dc41f6a76e"
                             , changeBlockNumber = Just 0
                             , changeAddress = "0x617e5941507aab5d2d8bcb56cb8c6ce2eeb16b21"
                             , changeData = "0x000000000000000000000000000000000000000000000000000000000000000a"
                             , changeTopics = ["0xa32bc18230dd172221ac5c4821a5f1f1a831f27b1396d244cdd891c58f132435"]
                             }
          in decodeEvent change `shouldBe` Right (NewCount 10)

      it "can decode erc20" $
         let ercchange = Change { changeLogIndex = Just "0x2"
                                , changeTransactionIndex = Just "0x2"
                                , changeTransactionHash = Just "0xe8cac6af0ceb3cecbcb2a5639361fc9811b1aa753672cf7c7e8b528df53e0e94"
                                , changeBlockHash = Just "0x0c7e1701858232ac210e3bcc8ab3b33cc6b08025692b22abb39059dc41f6a76e"
                                , changeBlockNumber = Just 0
                                , changeAddress = "0x617e5941507aab5d2d8bcb56cb8c6ce2eeb16b21"
                                , changeData = "0x000000000000000000000000000000000000000000000000000000000000000a"
                                , changeTopics = [ "0xb32bc18230dd172221ac5c4821a5f1f1a831f27b1396d244cdd891c58f132435"
                                                 , "0x0000000000000000000000000000000000000000000000000000000000000001"
                                                 , "0x0000000000000000000000000000000000000000000000000000000000000002"
                                                 ]
                                }
          in decodeEvent ercchange `shouldBe` Right (Transfer "0x0000000000000000000000000000000000000001" 10 "0x0000000000000000000000000000000000000002")

-- SimpleStorage Event Types

data NewCount = NewCount (UIntN 256) deriving (Eq, Show, GHC.Generic)
instance Generic NewCount


data NewCountIndexed = NewCountIndexed  deriving (Eq, Show, GHC.Generic)
instance Generic NewCountIndexed
instance AbiType NewCountIndexed where
    isDynamic = const False
instance AbiGet NewCountIndexed

data NewCountNonIndexed = NewCountNonIndexed (Tagged 1 (UIntN 256)) deriving (Eq, Show, GHC.Generic)
instance Generic NewCountNonIndexed
instance AbiType NewCountNonIndexed where
    isDynamic = const False
instance AbiGet NewCountNonIndexed

instance IndexedEvent NewCountIndexed NewCountNonIndexed NewCount where
  isAnonymous = const False

-- WeirdERC20 Event Types (Transfer type wrong order for testing purposes)
data Transfer = Transfer Address (UIntN 256) Address deriving (Eq, Show, GHC.Generic)
instance Generic Transfer

data TransferIndexed = TransferIndexed (Tagged 1 Address) (Tagged 3 Address) deriving (Eq, Show, GHC.Generic)
instance Generic TransferIndexed
instance AbiType TransferIndexed where
    isDynamic = const False
instance AbiGet TransferIndexed

data TransferNonIndexed = TransferNonIndexed (Tagged 2 (UIntN 256)) deriving (Eq, Show, GHC.Generic)
instance Generic TransferNonIndexed
instance AbiType TransferNonIndexed where
    isDynamic = const False
instance AbiGet TransferNonIndexed

instance IndexedEvent TransferIndexed TransferNonIndexed Transfer where
  isAnonymous = const False
