-- |
-- Module      :  Network.Ethereum.Web3.Utils
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  POSIX / WIN32
--
-- Web3 utility.
--
module Network.Ethereum.Web3.Util where

import Network.Ethereum.Web3.Internal (Filter(..), Call(..))
import qualified Network.Ethereum.Web3.Address as A (Address, toText)
import Data.HexString (toText, fromBytes, toBytes, hexString)
import Network.Ethereum.ContractAbi (Method, signature)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Text.Lazy.Builder.Int as B
import qualified Data.Text.Lazy.Builder as B
import Crypto.Hash (hash, Digest, Keccak_256)
import Network.Ethereum.Web3.Types (Web3)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Read as T
import qualified Data.Text as T
import Data.Monoid ((<>))
import Data.Text (Text)

hex :: Text -> Text
hex = toText . fromBytes . encodeUtf8

unhex :: Text -> Text
unhex = decodeUtf8 . toBytes . hexString . encodeUtf8

paddedText :: Text -> Text
paddedText t = hex t <> T.replicate x "0"
  where x = 64 - (T.length (hex t) `mod` 64)   -- 32 byte padding

paddedInt :: Integral a => a -> Text
paddedInt i = T.replicate x "0" <> v
  where v = LT.toStrict (B.toLazyText (B.hexadecimal i))
        x = 64 - (T.length v `mod` 64)   -- 32 byte padding

paddedAddr :: Text -> Text
paddedAddr a = T.replicate x "0" <> a
  where x = 64 - (T.length a `mod` 64)   -- 32 byte padding

text2data :: Text -> Text
text2data t = paddedInt (T.length (hex t) `div` 2) <> paddedText t

dataText :: Text -> Either String Text
dataText t = do
    (x, _) <- T.hexadecimal (T.take 64 t)
    return (unhex (T.take x (T.drop 64 t)))

sha3 :: Text -> Text
sha3 x = T.pack (show digest)
  where digest :: Digest Keccak_256
        digest = hash (encodeUtf8 x)

eventFilter :: A.Address -> Method -> Filter
eventFilter addr event =
    Filter (Just ("0x" <> A.toText addr)) (Just [Just topic0, Nothing]) Nothing Nothing
  where topic0 = "0x" <> sha3 (signature event)

methodId :: Method -> Text
methodId = ("0x" <>) . T.take 10 . sha3 . signature

-- | Ether to Wei converter
toWei :: Double -> Integer
toWei = round . (* 10^18)

-- | Wei to Ether converter
fromWei :: Integer -> Double
fromWei = (/ 10^18) . fromIntegral
