-- |
-- Module      :  Network.Polkadot.Metadata.Type
-- Copyright   :  Aleksandr Krupenkin 2016-2021
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Runtime metadata type and encoding.
--

module Network.Polkadot.Metadata.Type where

import           Codec.Scale.Class                     (Decode (..),
                                                        Encode (..))
import           Codec.Scale.Core                      ()
import           Data.Aeson                            (FromJSON (..),
                                                        ToJSON (..),
                                                        Value (String))
import           Data.Text                             (Text)

import           Network.Polkadot.Metadata.Type.Parser (sanitizeM)

-- | Sanitized name for metadata type.
newtype Type = Type { unType :: Text }
    deriving (Eq, Ord, Show)

instance FromJSON Type where
    parseJSON (String s) = return (Type s)
    parseJSON _          = fail "Type should be a string"

instance ToJSON Type where
    toJSON = toJSON . unType

instance Decode Type where
    get = fmap Type (sanitizeM =<< get)

instance Encode Type where
    put = put . unType
