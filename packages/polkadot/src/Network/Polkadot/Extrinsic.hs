{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Polkadot.Extrinsic
-- Copyright   :  Aleksandr Krupenkin 2016-2021
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- Extrinsic is a piece of data from external world.
--

module Network.Polkadot.Extrinsic where

import           Codec.Scale                 (encode)
import           Codec.Scale.Class           (Decode (..), Encode (..))
import           Data.ByteString             (ByteString)
import           Network.Polkadot.Primitives (MultiSignature)
--import           Network.Polkadot.Extrinsic.Signature (Signature)

-- | The third generation of compact extrinsics.
data Extrinsic = Extrinsic !ByteString !(Maybe MultiSignature)

instance Encode Extrinsic where
    put xt = put encoded  -- put lenght-prefixed extrinsic
      where
        encoded :: ByteString
        encoded = case xt of
            Extrinsic call Nothing ->
                let version = "\x04"  -- V4, signed bit unset
                 in version <> encode call
            Extrinsic call (Just sig) ->
                let version = "\x84"  -- V4, signed bit set
                 in version <> encode sig <> encode call
