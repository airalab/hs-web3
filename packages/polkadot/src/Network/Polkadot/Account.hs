{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :  Network.Polkadot.Account
-- Copyright   :  Aleksandr Krupenkin 2016-2021
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Polkadot account types.
--

module Network.Polkadot.Account where

import           Codec.Scale                 (decode, encode)
import           Control.Monad               ((<=<))
import           Data.BigNum                 (h256)
import           Data.Bits                   (bit, shiftL, shiftR, (.&.), (.|.))
import           Data.ByteArray              (convert)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as BS (drop, length, pack, take)
import           Data.Digest.Blake2          (blake2_256)
import           Data.Maybe                  (fromJust)
import           Data.Word                   (Word16)

import           Network.Polkadot.Primitives (MultiSigner (..))
import qualified Network.Polkadot.Primitives as P (AccountId)

-- | Some type that is able to be collapsed into an account ID.
--
-- It is not possible to recreate the original value from the account ID.
class IdentifyAccount a where
    -- | The account ID that this can be transformed into.
    type AccountId a

    -- | Transform into an account.
    into_account :: a -> AccountId a

instance IdentifyAccount MultiSigner where
    type AccountId MultiSigner = P.AccountId
    into_account (Ed25519Signer pub) = fromJust (h256 pub)
    into_account (EcdsaSigner pub)   = fromJust (h256 $ blake2_256 $ convert pub)
    into_account Sr25519Signer       = error "Sr25519 has no support yet"

instance Show MultiSigner where
    show = show . to_ss58check . into_account

-- | Key that can be encoded to/from SS58.
--
-- See <https://github.com/paritytech/substrate/wiki/External-Address-Format-(SS58)#address-type>
-- for information on the codec.
class Ss58Codec a where
    -- | Some if the string is a properly encoded SS58Check address (default prefix).
    from_ss58check :: ByteString -> Either String a
    from_ss58check = from_ss58check_with_version 42

    -- | Return the ss58-check string for this key (default prefix).
    to_ss58check :: a -> ByteString
    to_ss58check = to_ss58check_with_version 42

    -- | Return the ss58-check string for this key.
    to_ss58check_with_version :: Word16 -> a -> ByteString

    -- | Some if the string is a properly encoded SS58Check address (default prefix).
    from_ss58check_with_version :: Word16 -> ByteString -> Either String a

instance Ss58Codec P.AccountId where
    from_ss58check_with_version v = decode <=< from_ss58check_with_version' v <=< from_base58
    to_ss58check_with_version v = to_base58 . to_ss58check_with_version' v . encode

-- | TODO
to_base58 :: ByteString -> ByteString
to_base58 = id

-- | TODO
from_base58 :: ByteString -> Either String ByteString
from_base58 = return

to_ss58check_with_version' :: Word16 -> ByteString -> ByteString
to_ss58check_with_version' v input = encodeVersion v <> input <> ss58hash input

from_ss58check_with_version' :: Word16 -> ByteString -> Either String ByteString
from_ss58check_with_version' v input = versionGuard >> ss58hashGuard
  where
    checksumLen = 2
    versionLen | v < 64 = 1
               | otherwise = 2
    inputLen = BS.length input - checksumLen - versionLen
    input' = BS.take inputLen (BS.drop versionLen input)
    versionGuard
      | encodeVersion v == BS.take versionLen input = return ()
      | otherwise = Left "Bad version"
    ss58hashGuard
      | ss58hash input' == BS.drop (versionLen + inputLen) input = return input'
      | otherwise = Left "Bad checksum"

ss58hash :: ByteString -> ByteString
ss58hash = BS.take 2 . blake2_256 . ("SS58PRE" <>)

encodeVersion :: Word16 -> ByteString
encodeVersion v
  | v < 64 = BS.pack [fromIntegral v]
  | otherwise = let first = bit 6 .|. ((v `shiftR` 2) .&. 63)
                    second = (v `shiftR` 8) .|. ((v .&. 3) `shiftL` 6)
                 in BS.pack (fromIntegral <$> [first, second])
