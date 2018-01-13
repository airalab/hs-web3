{-# LANGUAGE PolyKinds #-}
-- |
-- Module      :  Network.Ethereum.Web3.Encoding
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Web3 ABI encoding data support.
--
module Network.Ethereum.Web3.Encoding (ABIEncode(..), ABIDecode(..)) where

import           Control.Error                           (hush)
import           Data.Monoid                             ((<>))
import           Data.Tagged                             (Tagged (..))
import           Data.Text                               (Text)
import qualified Data.Text                               as T
import qualified Data.Text.Lazy                          as LT
import           Data.Text.Lazy.Builder                  (Builder, fromLazyText,
                                                          fromText, toLazyText)
import           Network.Ethereum.Web3.Address           (Address)
import qualified Network.Ethereum.Web3.Address           as A
import           Network.Ethereum.Web3.Encoding.Internal
import           Text.Parsec                             (many1, parse)
import           Text.Parsec.Text.Lazy                   (Parser)

-- | Contract ABI data codec
class ABIEncode a where
    toDataBuilder  :: a -> Builder
    -- | Encode value into abi-encoding represenation
    toData :: a -> Text
    {-# INLINE toData #-}
    toData = LT.toStrict . toLazyText . toDataBuilder

class ABIDecode a where
    fromDataParser :: Parser a
    -- | Parse encoded value
    fromData :: Text -> Maybe a
    {-# INLINE fromData #-}
    fromData = hush . parse fromDataParser "" . LT.fromStrict

instance ABIEncode Bool where
    toDataBuilder  = int256HexBuilder . fromEnum

instance ABIDecode Bool where
    fromDataParser = fmap toEnum int256HexParser

instance ABIEncode Integer where
    toDataBuilder  = int256HexBuilder

instance ABIDecode Integer where
    fromDataParser = int256HexParser

instance ABIEncode Int where
    toDataBuilder  = int256HexBuilder

instance ABIDecode Int where
    fromDataParser = int256HexParser

instance ABIEncode Word where
    toDataBuilder  = int256HexBuilder

instance ABIDecode Word where
    fromDataParser = int256HexParser

instance ABIEncode Text where
    toDataBuilder  = textBuilder

instance ABIDecode Text where
    fromDataParser = textParser

instance ABIEncode Address where
    toDataBuilder  = alignR . fromText . A.toText

instance ABIDecode Address where
    fromDataParser = either error id . A.fromText
                     <$> (takeHexChar 24 *> takeHexChar 40)

instance ABIEncode a => ABIEncode [a] where
    toDataBuilder x = int256HexBuilder (length x)
                      <> foldMap toDataBuilder x

instance ABIDecode a => ABIDecode [a] where
    fromDataParser = do len <- int256HexParser
                        take len <$> many1 fromDataParser

instance ABIEncode a => ABIEncode (Tagged t a) where
  toDataBuilder (Tagged a) = toDataBuilder a

instance ABIDecode a => ABIDecode (Tagged t a) where
  fromDataParser = Tagged <$> fromDataParser
