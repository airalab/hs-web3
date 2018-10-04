{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Network.Ethereum.Contract.Method
-- Copyright   :  Alexander Krupenkin 2016-2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- Ethereum contract method support.
--

module Network.Ethereum.Contract.Method where

import           Data.Proxy                (Proxy)
import           Data.Solidity.Abi         (AbiPut, AbiType (..))
import           Data.Solidity.Abi.Generic ()
import           Data.Solidity.Prim.Bytes  (Bytes)

class AbiPut a => Method a where
    selector :: Proxy a -> Bytes

instance AbiType () where
    isDynamic _ = False

instance AbiPut ()

-- | Fallback contract method
instance Method () where
    selector _   = mempty
