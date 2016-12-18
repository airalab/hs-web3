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
module Network.Ethereum.Web3.Encoding (ABIEncoding(..)) where

import Data.Text.Lazy.Builder (Builder, toLazyText, fromText, fromLazyText)
import Data.Attoparsec.Text.Lazy (parse, maybeResult, Parser)
import qualified Network.Ethereum.Web3.Address as A
import qualified Data.Attoparsec.Text          as P
import qualified Data.Text.Lazy                as LT
import qualified Data.Text                     as T
import Network.Ethereum.Web3.Address (Address)
import Network.Ethereum.Web3.EncodingUtils
import Data.Monoid ((<>))
import Data.Text (Text)

-- | Contract ABI data codec
class ABIEncoding a where
    toDataBuilder  :: a -> Builder
    fromDataParser :: Parser a

    -- | Encode value into abi-encoding represenation
    toData :: a -> Text
    toData = LT.toStrict . toLazyText . toDataBuilder

    -- | Parse encoded value
    fromData :: Text -> Maybe a
    fromData = maybeResult . parse fromDataParser . LT.fromStrict

instance ABIEncoding Bool where
    toDataBuilder  = int256HexBuilder . fromEnum
    fromDataParser = fmap toEnum int256HexParser

instance ABIEncoding Integer where
    toDataBuilder  = int256HexBuilder
    fromDataParser = int256HexParser

instance ABIEncoding Int where
    toDataBuilder  = int256HexBuilder
    fromDataParser = int256HexParser

instance ABIEncoding Word where
    toDataBuilder  = int256HexBuilder
    fromDataParser = int256HexParser

instance ABIEncoding Text where
    toDataBuilder  = textBuilder
    fromDataParser = textParser

instance ABIEncoding Address where
    toDataBuilder  = alignR . fromText . A.toText
    fromDataParser = either error id . A.fromText
                     <$> (P.take 24 *> P.take 40)

instance ABIEncoding a => ABIEncoding [a] where
    toDataBuilder x = int256HexBuilder (length x)
                      <> foldMap toDataBuilder x
    fromDataParser = do len <- int256HexParser
                        take len <$> P.many1 fromDataParser
