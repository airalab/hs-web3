-- |
-- Module      :  Network.Ethereum.Contract.Method
-- Copyright   :  Aleksandr Krupenkin 2016-2021
-- License     :  Apache-2.0
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

-- | Smart contract method encoding
class AbiPut a => Method a where
    -- | Solidity function selector
    -- https://solidity.readthedocs.io/en/latest/abi-spec.html#function-selector-and-argument-encoding
    selector :: Proxy a -> Bytes

instance AbiType () where
    isDynamic _ = False

instance AbiPut ()

-- | Fallback contract method
instance Method () where
    selector _   = mempty
