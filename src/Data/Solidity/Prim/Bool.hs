{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      :  Data.Solidity.Prim.Bool
-- Copyright   :  Alexander Krupenkin 2016-2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  noportable
--
-- Ethereum Abi boolean type.
--

module Data.Solidity.Prim.Bool () where

import           Data.Solidity.Abi      (AbiGet (..), AbiPut (..), AbiType (..))
import           Data.Solidity.Prim.Int (getWord256, putWord256)

instance AbiType Bool where
    isDynamic _ = False

instance AbiGet Bool where
    abiGet = toEnum . fromIntegral <$> getWord256

instance AbiPut Bool where
    abiPut = putWord256 . fromIntegral . fromEnum
