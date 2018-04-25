-- |
-- Module      :  Network.Ethereum.ABI.Prim.String
-- Copyright   :  Alexander Krupenkin 2016-2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  noportable
--
-- Ethereum ABI UTF8-encoded string type.
--

module Network.Ethereum.ABI.Prim.String () where

import           Data.Text                       (Text)
import           Data.Text.Encoding              (decodeUtf8, encodeUtf8)

import           Network.Ethereum.ABI.Class      (ABIGet (..), ABIPut (..),
                                                  ABIType (..))
import           Network.Ethereum.ABI.Prim.Bytes ()

instance ABIType Text where
    isDynamic _ = True

instance ABIPut Text where
    abiPut = abiPut . encodeUtf8

instance ABIGet Text where
    abiGet = decodeUtf8 <$> abiGet
