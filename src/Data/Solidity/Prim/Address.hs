{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Data.Solidity.Prim.Address
-- Copyright   :  Alexander Krupenkin 2016-2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  noportable
--
-- Ethereum Abi address type.
--

module Data.Solidity.Prim.Address (
    Address
  , toHexString
  , fromHexString
  ) where

import           Control.Monad           ((<=<))
import           Data.Aeson              (FromJSON (..), ToJSON (..))
import           Data.ByteArray          (zero)
import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8   as C8 (drop, length)
import           Data.HexString          (HexString, fromBytes, hexString,
                                          toBytes, toText)
import           Data.String             (IsString (..))
import           Generics.SOP            (Generic)
import qualified GHC.Generics            as GHC (Generic)

import           Data.Solidity.Abi       (AbiGet (..), AbiPut (..),
                                          AbiType (..))
import           Data.Solidity.Abi.Codec (decode, encode)
import           Data.Solidity.Prim.Int  (UIntN)

-- | Ethereum account address
newtype Address = Address { unAddress :: UIntN 160 }
  deriving (Eq, Ord, GHC.Generic)

instance Generic Address

-- TODO: Address . drop 12 . sha3
{-
fromPublic :: ByteArrayAccess bin => bin -> Maybe Address
fromPublic = undefined
-}

fromHexString :: HexString -> Either String Address
fromHexString bs
  | bslen == 20 = decode (zero 12 <> toBytes bs :: ByteString)
  | otherwise = fail $ "Incorrect address length: " ++ show bslen
  where bslen = C8.length (toBytes bs)

toHexString :: Address -> HexString
toHexString = fromBytes . C8.drop 12 . encode

instance Show Address where
    show = show . toText . toHexString

instance IsString Address where
    fromString = either error id . (fromHexString <=< hexString) . fromString

instance AbiType Address where
    isDynamic _ = False

instance AbiGet Address where
    abiGet = Address <$> abiGet

instance AbiPut Address where
    abiPut = abiPut . unAddress

instance FromJSON Address where
    parseJSON = (either fail pure . fromHexString) <=< parseJSON

instance ToJSON Address where
    toJSON = toJSON . toHexString
