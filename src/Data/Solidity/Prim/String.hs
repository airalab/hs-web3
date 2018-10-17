-- |
-- Module      :  Data.Solidity.Prim.String
-- Copyright   :  Alexander Krupenkin 2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  noportable
--
-- Ethereum Abi UTF8-encoded string type.
--

module Data.Solidity.Prim.String where

import           Data.Text                (Text)
import           Data.Text.Encoding       (decodeUtf8, encodeUtf8)

import           Data.Solidity.Abi        (AbiGet (..), AbiPut (..),
                                           AbiType (..))
import           Data.Solidity.Prim.Bytes ()

instance AbiType Text where
    isDynamic _ = True

instance AbiPut Text where
    abiPut = abiPut . encodeUtf8

instance AbiGet Text where
    abiGet = decodeUtf8 <$> abiGet
