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
module Network.Ethereum.Web3.Encoding where

import Data.HexString (fromBytes, toBytes)
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Builder
import Data.Monoid ((<>))

-- | Make 256bit aligment; (left, right)
align :: ByteString -> (ByteString, ByteString)
align v = (v <> zeros, zeros <> v)
  where zerosLen = 64 - (L.length v `mod` 64)
        zeros = L.replicate zerosLen 0x30

-- | ABI data encoder/decoder
class ABIEncoding a where
    toData         :: a -> ByteString
    fromDataParser :: Parser a

    fromData :: ByteString -> Maybe a
    fromData = maybeResult . parse fromDataParser

instance ABIEncoding Integer where
    fromData = data2int
    toData   = int2data

instance ABIEncoding Int where
    fromData = data2int
    toData   = int2data

instance ABIEncoding Word where
    fromData = data2int
    toData   = int2data

instance ABIEncoding ByteString where
    fromData = data2bs
    toData   = bs2data

instance ABIEncoding Text where
    fromData = decodeUtf8 . fromData
    toData   = toData . encodeUtf8

int2data :: Integral a => a -> ByteString
int2data = snd . align . toLazyByteString
         . int64HexFixed . fromIntegral

data2int :: Num a => ByteString -> a
data2int = fromIntegral . 

bs2data :: ByteString -> ByteString
bs2data s = int2data (L.length s)
          <> fst (align (fromBytes s))

data2bs :: ByteString -> ByteString
data2bs d = (\l -> toBytes (L.take l str)) <$> len
  where len = data2int (L.take 64 d)
        str = L.drop 64 d

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
