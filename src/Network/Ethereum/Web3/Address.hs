-- |
-- Module      :  Network.Ethereum.Web3.Address
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unknown
--
-- Ethereum address type, render and parser.
--
module Network.Ethereum.Web3.Address (
    Address
  , fromText
  , toText
  , zero
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), Value(..))
import qualified Data.Char as C
import Data.Text.Lazy.Builder.Int as B (hexadecimal)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Read as R (hexadecimal)
import Data.Text (Text, unpack, pack)
import Data.String (IsString(..))
import Data.Text.Lazy (toStrict)
import qualified Data.Text as T
import Control.Monad ((<=<))
import Data.Monoid ((<>))

import GHC.Generics (Generic)

-- | Ethereum account address
newtype Address = Address { unAddress :: Integer }
  deriving (Eq, Ord, Generic)

instance Show Address where
    show = unpack . toText

instance IsString Address where
    fromString a = case fromText (pack a) of
        Right address -> address
        Left e -> error e

instance FromJSON Address where
    parseJSON (String a) = either fail return (fromText a)
    parseJSON _ = fail "Address should be a string"

instance ToJSON Address where
    toJSON = toJSON . ("0x" <>) . toText

-- | Parse 'Address' from text string
fromText :: Text -> Either String Address
fromText = fmap (Address . fst) . R.hexadecimal <=< check
  where check t | T.take 2 t == "0x" = check (T.drop 2 t)
                | otherwise = do if T.length t == 40 then pure () else lengthError
                                 if T.all C.isHexDigit t then pure () else invalidCharError
                                 pure t
        lengthError = Left "Invalid Address: text length not equal to 20"
        invalidCharError = Left "Invalid Address: contains non-hex character"

-- | Render 'Address' to text string
toText :: Address -> Text
toText = wFix . toStrict . toLazyText . B.hexadecimal . unAddress
  where wFix x | T.length x < 40 = T.replicate (40 - T.length x) "0" <> x
               | otherwise       = x

-- | Null address
zero :: Address
zero = Address 0
