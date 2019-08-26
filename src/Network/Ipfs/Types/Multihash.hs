
-- |
-- Module      :  Network.Ipfs.Types.Multihash
-- Copyright   :  Alexander Krupenkin 2016-2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unknown
--
-- Multihash Type Module
--

module Network.Ipfs.Types.Multihash where

import           Control.Applicative          ((<$>))
import           Data.Attoparsec.ByteString   (Parser, parseOnly)
import qualified Data.Attoparsec.ByteString    as A
import qualified Data.ByteString               as BS
import           Data.ByteString.Builder       (Builder, byteString, toLazyByteString)
import qualified Data.ByteString.Builder       as BB
import qualified Data.ByteString.Lazy          as BL
import           Data.Monoid                   ((<>))

data MultihashDigest = MultihashDigest
  { algorithm  :: !HashAlgorithm
  , length     :: !Length
  , digest     :: !Digest
  } deriving (Show, Eq)

type Length = Int

type Digest = BS.ByteString

data HashAlgorithm
  = SHA1
  | SHA256
  | SHA512
  | SHA3
  | BLAKE2B
  | BLAKE2S
  deriving (Show, Read, Eq, Enum, Bounded)

fromCode :: Int -> HashAlgorithm
fromCode 0x11 = SHA1
fromCode 0x12 = SHA256
fromCode 0x13 = SHA512
fromCode 0x14 = SHA3
fromCode 0x40 = BLAKE2B
fromCode 0x41 = BLAKE2S
fromCode _ = error "Unknown hash function code"

toCode :: HashAlgorithm -> Int
toCode SHA1 = 0x11
toCode SHA256 = 0x12
toCode SHA512 = 0x13
toCode SHA3 = 0x14
toCode BLAKE2B = 0x40
toCode BLAKE2S = 0x41

encode :: HashAlgorithm -> Digest -> BL.ByteString
encode h d = toLazyByteString $ encoder h d

encoder :: HashAlgorithm -> Digest -> Builder
encoder h d =
  (BB.word8 . fromIntegral $ toCode h) <>
  (BB.word8 . fromIntegral $ BS.length d) <>
  byteString d

decode :: BS.ByteString -> Either String MultihashDigest
decode = parseOnly decoder

decoder :: Parser MultihashDigest
decoder = do
  h <- (fromCode . fromIntegral <$> A.anyWord8)
  l <- (fromIntegral <$> A.anyWord8)
  d <- A.take l
  return $ MultihashDigest h l d