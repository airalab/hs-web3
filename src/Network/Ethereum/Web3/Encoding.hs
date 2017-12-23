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

import Data.Text.Lazy.Builder (Builder, toLazyText, fromText, fromLazyText)
import Data.Attoparsec.Text.Lazy (parse, maybeResult, Parser)
import qualified Network.Ethereum.Web3.Address as A
import qualified Data.Attoparsec.Text          as P
import qualified Data.Text.Lazy                as LT
import qualified Data.Text                     as T
import Network.Ethereum.Web3.Encoding.Internal
import Network.Ethereum.Web3.Address (Address)
import Data.Monoid ((<>))
import Data.Text (Text)

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
    fromData = maybeResult . parse fromDataParser . LT.fromStrict

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
                     <$> (P.take 24 *> P.take 40)

instance ABIEncode a => ABIEncode [a] where
    toDataBuilder x = int256HexBuilder (length x)
                      <> foldMap toDataBuilder x

instance ABIDecode a => ABIDecode [a] where
    fromDataParser = do len <- int256HexParser
                        take len <$> P.many1 fromDataParser
