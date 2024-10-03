{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

-- |
-- Module      :  Data.Solidity.Abi
-- Copyright   :  Aleksandr Krupenkin 2016-2024
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  noportable
--
-- The Application Binary Interface is the standard way to interact with contracts
-- in the Ethereum ecosystem, both from outside the blockchain and for contract-to-contract
-- interaction. Data is encoded according to its type, as described in this specification.
-- The encoding is not self describing and thus requires a schema in order to decode.
--

module Data.Solidity.Abi where

import           Data.Proxy     (Proxy)
import           Data.Serialize (Get, Putter)
import           Generics.SOP   (Generic, Rep, from, to)

-- | A class for abi encoding datatype descriptions
class AbiType a where
    isDynamic :: Proxy a -> Bool

-- | A class for encoding datatypes to their abi encoding
--
-- If your compiler has support for the @DeriveGeneric@ and
-- @DefaultSignatures@ language extensions (@ghc >= 7.2.1@),
-- the 'abiPut' method will have default generic implementations.
--
-- To use this option, simply add a @deriving 'Generic'@ clause
-- to your datatype and declare a 'AbiPut' instance for it without
-- giving a definition for 'abiPut'.
--
class AbiType a => AbiPut a where
    abiPut :: Putter a

    default abiPut :: (Generic a, Rep a ~ rep, GenericAbiPut rep) => Putter a
    abiPut = gAbiPut . from

-- | A class for encoding generically composed datatypes to their abi encoding
class GenericAbiPut a where
    gAbiPut :: Putter a

-- | A class for decoding datatypes from their abi encoding
--
-- If your compiler has support for the @DeriveGeneric@ and
-- @DefaultSignatures@ language extensions (@ghc >= 7.2.1@),
-- the 'abiGet' method will have default generic implementations.
--
-- To use this option, simply add a @deriving 'Generic'@ clause
-- to your datatype and declare a 'AbiGet' instance for it without
-- giving a definition for 'abiGet'.
--
class AbiType a => AbiGet a where
    abiGet :: Get a

    default abiGet :: (Generic a, Rep a ~ rep, GenericAbiGet rep) => Get a
    abiGet = to <$> gAbiGet

-- | A class for decoding generically composed datatypes from their abi encoding
class GenericAbiGet a where
    gAbiGet :: Get a
