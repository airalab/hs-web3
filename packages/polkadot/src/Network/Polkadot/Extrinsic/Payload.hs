-- |
-- Module      :  Network.Polkadot.Extrinsic.Payload
-- Copyright   :  Aleksandr Krupenkin 2016-2021
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Extrinsic payload and payload signing methods.
--

module Network.Polkadot.Extrinsic.Payload where

import           Codec.Scale                                (encode)
import           Codec.Scale.Class                          (Encode (..))
import           Data.ByteString                            (ByteString)
import qualified Data.ByteString                            as BS (length)
import           Data.Digest.Blake2                         (blake2_256)
import           Network.JsonRpc.TinyClient                 (JsonRpc)

import           Network.Polkadot.Crypto                    (MultiPair (MultiSignature, multi_sign))
import           Network.Polkadot.Extrinsic.SignedExtension (SignedExtension (..))

-- | A payload that has been signed for an unchecked extrinsics.
--
-- Note that the payload that we sign to produce unchecked extrinsic signature
-- is going to be different than the `Payload` - so the thing the extrinsic
-- actually contains.
type Payload call extra = (call, extra)

-- | Sign extrinsic's payload by multi-pair.
sign_payload :: (MultiPair a, Encode c, SignedExtension e, JsonRpc m)
             => a
             -> Payload c e
             -> m (MultiSignature a)
sign_payload pair (call, extra) = do
    additional <- additional_signed extra
    let encoded = encode (call, extra, additional)
        payload | BS.length encoded > 256 = blake2_256 encoded
                | otherwise = encoded
    return $ multi_sign pair (encode payload :: ByteString)
