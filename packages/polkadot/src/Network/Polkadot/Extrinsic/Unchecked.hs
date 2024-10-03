{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Network.Polkadot.Extrinsic.Unchecked
-- Copyright   :  Aleksandr Krupenkin 2016-2024
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- Extrinsic is a piece of data from external world.
--

module Network.Polkadot.Extrinsic.Unchecked
    ( UncheckedExtrinsic (..)
    , new_unsigned
    , new_signed
    , sign_extrinsic
    ) where

import           Codec.Scale                                (encode)
import           Codec.Scale.Class                          (Decode (..),
                                                             Encode (..))
import           Control.Arrow                              ((&&&))
import           Control.Monad                              (when)
import           Data.Bits                                  (clearBit, setBit,
                                                             testBit)
import           Data.ByteArray.HexString                   (HexString)
import           Data.ByteString                            (ByteString)
import           Data.Word                                  (Word8)
import           Network.JsonRpc.TinyClient                 (JsonRpc)

import           Network.Polkadot.Crypto                    (MultiPair (..))
import           Network.Polkadot.Extrinsic.Payload         (sign_payload)
import           Network.Polkadot.Extrinsic.SignedExtension (SignedExtension)

-- | Current version of the 'UncheckedExtrinsic' format.
extrinsic_version :: Word8
extrinsic_version = 4

-- | A extrinsic right from the external world. This is unchecked and so
-- can contain a signature.
data UncheckedExtrinsic c a s e
    = UncheckedExtrinsic
    { extrinsicSignature :: !(Maybe (a, s, e))
      -- ^ The signature, address, number of extrinsics have come before from
      -- the same signer and an era describing the longevity of this transaction,
      -- if this is a signed extrinsic.
    , extrinsicFunction  :: !c
      -- ^ The function that should be called.
    }

instance Encode c => Show (UncheckedExtrinsic c a b c) where
    show (UncheckedExtrinsic _ call) = "UncheckedExtrinsic " ++ show encoded
      where
        encoded :: HexString
        encoded = encode call

instance (Encode c, Encode a, Encode s, Encode e) => Encode (UncheckedExtrinsic c a s e) where
    put xt = put encoded
      where
        encoded :: ByteString
        encoded = case xt of
            UncheckedExtrinsic Nothing call
                -> encode extrinsic_version <> encode call
            UncheckedExtrinsic (Just s) call
                -> encode (setBit extrinsic_version 7) <> encode s <> encode call

instance (Decode c, Decode a, Decode s, Decode e) => Decode (UncheckedExtrinsic c a s e) where
    get = do
        (_v :: [()]) <- get
        (signed, version) <- (flip testBit 7 &&& flip clearBit 7) <$> get
        when (version /= extrinsic_version) $ fail "bad version"
        UncheckedExtrinsic
            <$> (if signed then fmap Just get else return Nothing)
            <*> get

-- | New instance of a signed extrinsic aka "transaction".
new_signed :: c -> a -> s -> e -> UncheckedExtrinsic c a s e
new_signed call address sig extra = UncheckedExtrinsic (Just (address, sig, extra)) call

-- | New instance of an unsigned extrinsic aka "inherent".
new_unsigned :: f -> UncheckedExtrinsic f a b c
new_unsigned = UncheckedExtrinsic Nothing

-- | Create and sign extrinsic by account.
sign_extrinsic :: (MultiPair a, Encode c, SignedExtension e, JsonRpc m)
               => a
               -- ^ Account to sign extrinsic.
               -> c
               -- ^ Function to call on runtime.
               -> e
               -- ^ Additional data to sign like nonce, blockhash, etc.
               -> m (UncheckedExtrinsic c (MultiAddress a) (MultiSignature a) e)
sign_extrinsic a c e = do
    sig <- sign_payload a (c, e)
    return $ new_signed c (multi_address a) sig e
