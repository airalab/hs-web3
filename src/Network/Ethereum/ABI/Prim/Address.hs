{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      :  Network.Ethereum.ABI.Prim.Address
-- Copyright   :  Alexander Krupenkin 2016-2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  noportable
--
-- Ethereum ABI address type.
--

module Network.Ethereum.ABI.Prim.Address (
    Address
  ) where

import           Control.Monad                 ((<=<))
import           Data.Aeson                    (FromJSON (..), ToJSON (..),
                                                Value (String))
import           Data.ByteArray                (ByteArrayAccess, Bytes, zero)
import           Data.ByteArray.Encoding       (Base (Base16), convertFromBase,
                                                convertToBase)
import           Data.ByteString               (ByteString)
import qualified Data.ByteString.Char8         as C8 (drop, pack, take, unpack)
import           Data.Monoid                   ((<>))
import           Data.String                   (IsString (..))
import           Data.Text.Encoding            (decodeUtf8, encodeUtf8)
import           Generics.SOP                  (Generic)
import qualified GHC.Generics                  as GHC (Generic)

import           Network.Ethereum.ABI.Class    (ABIGet (..), ABIPut (..),
                                                ABIType (..))
import           Network.Ethereum.ABI.Codec    (decode, encode)
import           Network.Ethereum.ABI.Prim.Int (UIntN)

-- | Ethereum account address
newtype Address = Address { unAddress :: UIntN 160 }
  deriving (Eq, Ord, GHC.Generic)

instance Generic Address

-- TODO: Address . drop 12 . sha3
{-
fromPublic :: ByteArrayAccess bin => bin -> Maybe Address
fromPublic = undefined
-}

fromHexString :: ByteString -> Either String Address
fromHexString = decode . align <=< convertFromBase Base16 . trim0x
  where trim0x s | C8.take 2 s == "0x" = C8.drop 2 s
                 | otherwise = s
        align = (zero 12 <>) :: Bytes -> Bytes

toHexString :: Address -> ByteString
toHexString = ("0x" <>) . convertToBase Base16 . C8.drop 12 . encode

instance Show Address where
    show = C8.unpack . toHexString

instance IsString Address where
    fromString = either error id . fromHexString . C8.pack

instance ABIType Address where
    isDynamic _ = False

instance ABIGet Address where
    abiGet = Address <$> abiGet

instance ABIPut Address where
    abiPut = abiPut . unAddress

instance FromJSON Address where
    parseJSON (String a) = either fail return $ fromHexString (encodeUtf8 a)
    parseJSON _          = fail "Address should be a string"

instance ToJSON Address where
    toJSON = String . decodeUtf8 . toHexString
