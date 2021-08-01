{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

-- |
-- Module      :  Network.Polkadot.Extrinsic.Signature
-- Copyright   :  Aleksandr Krupenkin 2016-2021
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Extrinsic payload signing and signature types.
--

module Network.Polkadot.Extrinsic.Signature where

import           Codec.Scale             (Decode, encode)
import           Codec.Scale.Class       (Encode (..))
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as BS (length)
import           Data.Digest.Blake2      (blake2_256)

import           Network.Polkadot.Crypto (MultiPair (MultiSignature, multi_sign))

-- | A payload that has been signed for an unchecked extrinsics.
--
-- Note that the payload that we sign to produce unchecked extrinsic signature
-- is going to be different than the `SignaturePayload` - so the thing the extrinsic
-- actually contains.
newtype SignedPayload call extra = SignedPayload (call, extra)

instance (Encode c, SignedExtension e) => Encode (SignedPayload c e) where
    put (SignedPayload (call, extra))
        | BS.length encoded > 256 = put (blake2_256 encoded)
        | otherwise = put encoded
      where
        encoded = encode (call, extra, additional_signed extra)

-- | Means by which a transaction may be extended. This type embodies both the data and the logic
-- that should be additionally associated with the transaction. It should be plain old data.
class (Encode a, Decode a, Encode (AdditionalSigned a), Decode (AdditionalSigned a))
    => SignedExtension a where
    -- | Any additional data that will go into the signed payload. This may be created dynamically
    --  from the transaction using the `additional_signed` function.
    type AdditionalSigned a

    -- | Construct any additional data that should be in the signed payload of the transaction. Can
    -- also perform any pre-signature-verification checks and return an error if needed.
    additional_signed :: a -> AdditionalSigned a

instance SignedExtension () where
    type AdditionalSigned () = ()
    additional_signed _ = ()

instance (SignedExtension a, SignedExtension b) => SignedExtension (a, b) where
    type AdditionalSigned (a, b) = (AdditionalSigned a, AdditionalSigned b)
    additional_signed (a, b) = (additional_signed a, additional_signed b)

instance (SignedExtension a, SignedExtension b, SignedExtension c) => SignedExtension (a, b, c) where
    type AdditionalSigned (a, b, c) = (AdditionalSigned a, AdditionalSigned b, AdditionalSigned c)
    additional_signed (a, b, c) = (additional_signed a, additional_signed b, additional_signed c)

instance (SignedExtension a, SignedExtension b, SignedExtension c, SignedExtension d) => SignedExtension (a, b, c, d) where
    type AdditionalSigned (a, b, c, d) = (AdditionalSigned a, AdditionalSigned b, AdditionalSigned c, AdditionalSigned d)
    additional_signed (a, b, c, d) = (additional_signed a, additional_signed b, additional_signed c, additional_signed d)

instance (SignedExtension a, SignedExtension b, SignedExtension c, SignedExtension d, SignedExtension e) => SignedExtension (a, b, c, d, e) where
    type AdditionalSigned (a, b, c, d, e) = (AdditionalSigned a, AdditionalSigned b, AdditionalSigned c, AdditionalSigned d, AdditionalSigned e)
    additional_signed (a, b, c, d, e) = (additional_signed a, additional_signed b, additional_signed c, additional_signed d, additional_signed e)

instance (SignedExtension a, SignedExtension b, SignedExtension c, SignedExtension d, SignedExtension e, SignedExtension f) => SignedExtension (a, b, c, d, e, f) where
    type AdditionalSigned (a, b, c, d, e, f) = (AdditionalSigned a, AdditionalSigned b, AdditionalSigned c, AdditionalSigned d, AdditionalSigned e, AdditionalSigned f)
    additional_signed (a, b, c, d, e, f) = (additional_signed a, additional_signed b, additional_signed c, additional_signed d, additional_signed e, additional_signed f)

instance (SignedExtension a, SignedExtension b, SignedExtension c, SignedExtension d, SignedExtension e, SignedExtension f, SignedExtension g) => SignedExtension (a, b, c, d, e, f, g) where
    type AdditionalSigned (a, b, c, d, e, f, g) = (AdditionalSigned a, AdditionalSigned b, AdditionalSigned c, AdditionalSigned d, AdditionalSigned e, AdditionalSigned f, AdditionalSigned g)
    additional_signed (a, b, c, d, e, f, g) = (additional_signed a, additional_signed b, additional_signed c, additional_signed d, additional_signed e, additional_signed f, additional_signed g)


-- | Sign extrinsic payload by account.
sign_payload :: (MultiPair a, Encode c, SignedExtension e)
             => a
             -> SignedPayload c e
             -> MultiSignature a
sign_payload pair payload = multi_sign pair (encode payload :: ByteString)
