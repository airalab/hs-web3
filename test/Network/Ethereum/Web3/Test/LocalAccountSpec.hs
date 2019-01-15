{-# LANGUAGE OverloadedStrings #-}

-- Module      :  Network.Ethereum.Web3.Test.LocalAccountSpec
-- Copyright   :  Alexander Krupenkin 2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- Simple local account transaction test.
--

module Network.Ethereum.Web3.Test.LocalAccountSpec where

import           Lens.Micro                       ((.~))
import           Lens.Micro.Mtl                   ((.=))
import           Test.Hspec

import           Crypto.Ethereum.Utils            (derivePubKey, importKey)
import           Data.ByteArray.HexString         (HexString)
import           Data.Solidity.Prim.Address       (fromPubKey)
import           Network.Ethereum.Account         (LocalKey (..), send, to,
                                                   value, withAccount,
                                                   withParam)
import           Network.Ethereum.Api.Eth         (getBalance)
import           Network.Ethereum.Api.Types       (DefaultBlock (Pending))
import           Network.Ethereum.Unit            (Ether, toWei)
import           Network.Ethereum.Web3.Test.Utils (web3)

spec :: Spec
spec = describe "Local account transactions" $ do
    it "should send value" $ do
        let key = "0x0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20" :: HexString
            local = LocalKey (importKey key) 420123
            localAddress = fromPubKey (derivePubKey $ importKey key)
            dest = "0x0000000000000000000000000000000000000042"

        -- Prepare
        web3 $ withAccount () $
            withParam (to .~ localAddress) $ do
                value .= (1 :: Ether)
                send ()

        balance <- web3 $ do
            withAccount local $
                withParam (to .~ dest) $ do
                    value .= (0.5 :: Ether)
                    send ()
            getBalance dest Pending

        fromIntegral balance `shouldBe` toWei (0.5 :: Ether)
