-- |
-- Module      :  Network.Ethereum.Web3.Encoding
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  POSIX / WIN32
--
-- Web3 ABI encoding data support.
--
module Network.Ethereum.Web3.Encoding (ABIEncoding(..)) where

import Data.Text.Lazy.Builder (Builder, toLazyText, fromText, fromLazyText)
import Data.Attoparsec.Text.Lazy (parse, maybeResult, Parser)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Network.Ethereum.Address as A
import qualified Data.Attoparsec.Text as P
import Network.Ethereum.Address (Address)
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder.Int as B
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Read as R
import qualified Data.HexString as H
import qualified Data.Text as T
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Bits (Bits)

-- | ABI data encoder/decoder
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

-- | Make 256bit aligment; lazy (left, right)
align :: Builder -> (Builder, Builder)
align v = (v <> zeros, zeros <> v)
  where zerosLen = 64 - (LT.length s `mod` 64)
        zeros = fromLazyText (LT.replicate zerosLen "0")
        s = toLazyText v

alignL, alignR :: Builder -> Builder
{-# INLINE alignL #-}
alignL = fst . align
{-# INLINE alignR #-}
alignR = snd . align

int256HexBuilder :: Integral a => a -> Builder
int256HexBuilder = alignR . B.hexadecimal

int256HexParser :: (Bits a, Integral a) => Parser a
int256HexParser = do
    hex <- P.take 64
    case R.hexadecimal hex of
        Right (v, "") -> return v
        _ -> fail ("Broken hexadecimal: `" ++ T.unpack hex ++ "`")

textBuilder :: Text -> Builder
textBuilder s = int256HexBuilder (T.length hex `div` 2)
             <> alignL (fromText hex)
  where textToHex = H.toText . H.fromBytes . encodeUtf8
        hex = textToHex s

textParser :: Parser Text
textParser = do
    len <- int256HexParser
    let zeroBytes = 32 - (len `mod` 32)
    str <- P.take (len * 2) <* P.take (zeroBytes * 2)
    return (hexToText str)
  where hexToText = decodeUtf8 . H.toBytes . H.hexString . encodeUtf8
