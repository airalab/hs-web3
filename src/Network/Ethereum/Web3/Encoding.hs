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
module Network.Ethereum.Web3.Encoding (
    ABIEncoding(..)
  ) where

import Data.Text.Lazy.Builder (Builder, toLazyText, fromLazyText, fromText)
import Data.Attoparsec.Text.Lazy (parse, maybeResult, Parser)
import qualified Network.Ethereum.Address as A
import qualified Data.Text.Lazy.Encoding as LT
import Network.Ethereum.Web3.Util (textHex)
import Network.Ethereum.Address (Address)
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder.Int as B
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import Data.Text.Read as R
import Data.Text (Text)
import Data.Monoid ((<>))

-- | ABI data encoder/decoder
class ABIEncoding a where
    toDataBuilder  :: a -> Builder
    fromDataParser :: Parser a

    toData :: a -> Text
    toData = LT.toStrict . toLazyText . toDataBuilder

    fromData :: Text -> Maybe a
    fromData = maybeResult . parse fromDataParser . LT.fromStrict

instance ABIEncoding Integer where
    toDataBuilder  = int256HexFixed
    fromDataParser = undefined

instance ABIEncoding Int where
    toDataBuilder  = int256HexFixed
    fromDataParser = undefined

instance ABIEncoding Word where
    toDataBuilder  = int256HexFixed
    fromDataParser = undefined

instance ABIEncoding Text where
    toDataBuilder  = encodeText
    fromDataParser = undefined

instance ABIEncoding Address where
    fromDataParser = undefined
    toDataBuilder  = alignR . fromText . A.toText

-- | Make 256bit aligment; lazy (left, right)
align :: Builder -> (Builder, Builder)
align v = (v <> zeros, zeros <> v)
  where zerosLen = 64 - (LT.length s `mod` 64)
        zeros = fromLazyText (LT.replicate zerosLen "0")
        s = toLazyText v

alignL, alignR :: Builder -> Builder
alignL = fst . align
alignR = snd . align

int256HexFixed :: Integral a => a -> Builder
int256HexFixed = alignR . B.hexadecimal

encodeText :: Text -> Builder
encodeText s = int256HexFixed (T.length s) <> alignL (textHex s)

{-
paddedText :: Text -> Text
paddedText t = hex t <> T.replicate x "0"
  where x = 64 - (T.length (hex t) `mod` 64)   -- 32 byte padding


paddedAddr :: Address -> Text
paddedAddr a = T.replicate x "0" <> A.toText a
  where x = 64 - (T.length a `mod` 64)   -- 32 byte padding

textData :: Text -> Text
textData t = paddedInt (T.length t) <> paddedText t

dataText :: Text -> Either String Text
dataText t = do
    (x, _) <- T.hexadecimal (T.take 64 t)
    return (unhex (T.take x (T.drop 64 t)))
-}
