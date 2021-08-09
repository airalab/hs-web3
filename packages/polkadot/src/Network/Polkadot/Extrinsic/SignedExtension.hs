{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

-- |
-- Module      :  Network.Polkadot.Extrinsic.SignedExtension
-- Copyright   :  Aleksandr Krupenkin 2016-2021
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Additional data that could be attached to the transaction.
--

module Network.Polkadot.Extrinsic.SignedExtension where


import           Codec.Scale                (Decode, Encode)
import           Network.JsonRpc.TinyClient (JsonRpc)

-- | Means by which a transaction may be extended. This type embodies both the data and the logic
-- that should be additionally associated with the transaction. It should be plain old data.
class (Encode a, Decode a, Encode (AdditionalSigned a), Decode (AdditionalSigned a))
    => SignedExtension a where
    -- | Any additional data that will go into the signed payload. This may be created dynamically
    --  from the transaction using the `additional_signed` function.
    type AdditionalSigned a

    -- | Construct any additional data that should be in the signed payload of the transaction. Can
    -- also perform any pre-signature-verification checks and return an error if needed.
    additional_signed :: JsonRpc m => a -> m (AdditionalSigned a)

instance SignedExtension () where
    type AdditionalSigned () = ()
    additional_signed _ = return ()

instance (SignedExtension a, SignedExtension b) => SignedExtension (a, b) where
    type AdditionalSigned (a, b) = (AdditionalSigned a, AdditionalSigned b)
    additional_signed (a, b) = (,)
        <$> additional_signed a
        <*> additional_signed b

instance (SignedExtension a, SignedExtension b, SignedExtension c) => SignedExtension (a, b, c) where
    type AdditionalSigned (a, b, c) = (AdditionalSigned a, AdditionalSigned b, AdditionalSigned c)
    additional_signed (a, b, c) = (,,)
        <$> additional_signed a
        <*> additional_signed b
        <*> additional_signed c

instance (SignedExtension a, SignedExtension b, SignedExtension c, SignedExtension d) => SignedExtension (a, b, c, d) where
    type AdditionalSigned (a, b, c, d) = (AdditionalSigned a, AdditionalSigned b, AdditionalSigned c, AdditionalSigned d)
    additional_signed (a, b, c, d) = (,,,)
        <$> additional_signed a
        <*> additional_signed b
        <*> additional_signed c
        <*> additional_signed d

instance (SignedExtension a, SignedExtension b, SignedExtension c, SignedExtension d, SignedExtension e) => SignedExtension (a, b, c, d, e) where
    type AdditionalSigned (a, b, c, d, e) = (AdditionalSigned a, AdditionalSigned b, AdditionalSigned c, AdditionalSigned d, AdditionalSigned e)
    additional_signed (a, b, c, d, e) = (,,,,)
        <$> additional_signed a
        <*> additional_signed b
        <*> additional_signed c
        <*> additional_signed d
        <*> additional_signed e

instance (SignedExtension a, SignedExtension b, SignedExtension c, SignedExtension d, SignedExtension e, SignedExtension f) => SignedExtension (a, b, c, d, e, f) where
    type AdditionalSigned (a, b, c, d, e, f) = (AdditionalSigned a, AdditionalSigned b, AdditionalSigned c, AdditionalSigned d, AdditionalSigned e, AdditionalSigned f)
    additional_signed (a, b, c, d, e, f) = (,,,,,)
        <$> additional_signed a
        <*> additional_signed b
        <*> additional_signed c
        <*> additional_signed d
        <*> additional_signed e
        <*> additional_signed f

instance (SignedExtension a, SignedExtension b, SignedExtension c, SignedExtension d, SignedExtension e, SignedExtension f, SignedExtension g) => SignedExtension (a, b, c, d, e, f, g) where
    type AdditionalSigned (a, b, c, d, e, f, g) = (AdditionalSigned a, AdditionalSigned b, AdditionalSigned c, AdditionalSigned d, AdditionalSigned e, AdditionalSigned f, AdditionalSigned g)
    additional_signed (a, b, c, d, e, f, g) = (,,,,,,)
        <$> additional_signed a
        <*> additional_signed b
        <*> additional_signed c
        <*> additional_signed d
        <*> additional_signed e
        <*> additional_signed f
        <*> additional_signed g
