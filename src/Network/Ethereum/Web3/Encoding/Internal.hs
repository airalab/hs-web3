{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      :  Network.Ethereum.Web3.Encoding.Internal
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- ABI encoding internal function and types.
--
module Network.Ethereum.Web3.Encoding.Internal where

import           Control.Monad                 (replicateM)
import           Data.Bits                     (Bits)
import qualified Data.ByteString.Base16        as BS16 (decode, encode)
import qualified Data.ByteString.Base16        as BS16 (decode, encode)
import           Data.Monoid                   ((<>))
import           Data.Proxy                    (Proxy (..))
import           Data.Tagged                   (Tagged)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Text.Encoding            (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy                as LT
import           Data.Text.Lazy.Builder        (Builder, fromLazyText, fromText,
                                                toLazyText)
import           Data.Text.Lazy.Builder        (Builder, fromLazyText, fromText,
                                                toLazyText)
import           Data.Text.Lazy.Builder.Int    as B
import qualified Data.Text.Read                as R
import qualified Text.Parsec                   as P
import           Text.Parsec.Text.Lazy         (Parser)

import           Network.Ethereum.Web3.Address (Address)

class EncodingType a where
    typeName :: Proxy a -> String
    isDynamic :: Proxy a -> Bool

instance EncodingType Bool where
    typeName  = const "bool"
    isDynamic = const False

instance EncodingType Integer where
    typeName  = const "int"
    isDynamic = const False

instance EncodingType Int where
    typeName  = const "int"
    isDynamic = const False

instance EncodingType Word where
    typeName  = const "uint"
    isDynamic = const False

instance EncodingType Text where
    typeName  = const "string"
    isDynamic = const True

instance EncodingType Address where
    typeName  = const "address"
    isDynamic = const False

instance EncodingType a => EncodingType [a] where
    typeName  = const "[]"
    isDynamic = const True

instance EncodingType a => EncodingType (Tagged t a) where
    typeName _ = typeName (Proxy :: Proxy a)
    isDynamic _ = isDynamic (Proxy :: Proxy a)

-- | Make 256bit alignment; lazy (left, right)
align :: Builder -> (Builder, Builder)
align v = (v <> zeros, zeros <> v)
  where zerosLen | LT.length s `mod` 64 == 0 = 0
                 | otherwise = 64 - (LT.length s `mod` 64)
        zeros = fromLazyText (LT.replicate zerosLen "0")
        s = toLazyText v

-- | Left/Right specialized alignment
alignL, alignR :: Builder -> Builder
{-# INLINE alignL #-}
alignL = fst . align
{-# INLINE alignR #-}
alignR = snd . align

toQuantityHexText :: Integral a => a -> Text
toQuantityHexText = T.append "0x" . LT.toStrict . toLazyText . B.hexadecimal

int256HexBuilder :: Integral a => a -> Builder
int256HexBuilder x
  | x < 0 = int256HexBuilder (2^256 + fromIntegral x)
  | otherwise = alignR (B.hexadecimal x)

int256HexParser :: (Bits a, Integral a) => Parser a
int256HexParser = do
    hex <- takeHexChar 64
    case R.hexadecimal hex of
        Right (v, "") -> return v
        _ -> fail ("Broken hexadecimal: `" ++ T.unpack hex ++ "`")

textBuilder :: Text -> Builder
textBuilder s = int256HexBuilder (T.length hex `div` 2)
             <> alignL (fromText hex)
  where textToHex = decodeUtf8 . BS16.encode . encodeUtf8
        hex = textToHex s

textParser :: Parser Text
textParser = do
    len <- int256HexParser
    let zeroBytes = 32 - (len `mod` 32)
    str <- takeHexChar (len * 2) <* takeHexChar (zeroBytes * 2)
    return (hexToText str)
  where hexToText = decodeUtf8 . fst . BS16.decode . encodeUtf8

takeHexChar :: Int -> Parser T.Text
takeHexChar n = LT.toStrict . LT.pack <$> replicateM n P.anyChar
