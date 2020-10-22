{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

-- |
-- Module      :  Network.Polkadot.Metadata.MagicNumber
-- Copyright   :  Aleksandr Krupenkin 2016-2020
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Metadata V9 data type.
--

module Network.Polkadot.Metadata.MagicNumber where

import           Codec.Scale.Class (Decode (..), Encode (..))
import           Codec.Scale.Core  ()
import           Control.Monad     (when)
import           Data.Aeson        (FromJSON (..), ToJSON (..), Value (Number))
import           Data.Word         (Word32)

-- | `meta`, reversed for Little Endian encoding
magic_number :: Word32
magic_number = 0x6174656d

-- | 32-bit prefix magic sentence.
data MagicNumber = MagicNumber
    deriving (Eq, Show)

instance Decode MagicNumber where
    get = do
        n <- get
        when (n /= magic_number) $
            fail "Bad magic number"
        return MagicNumber

instance Encode MagicNumber where
    put _ = put magic_number

instance FromJSON MagicNumber where
    parseJSON (Number n) = do
        when (n /= fromIntegral magic_number) $
            fail "Bad magic number"
        return MagicNumber
    parseJSON _ = fail "Magic number should be a number"

instance ToJSON MagicNumber where
    toJSON _ = toJSON magic_number
