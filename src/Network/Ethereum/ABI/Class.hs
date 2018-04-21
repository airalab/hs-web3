{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}

-- |
-- Module      :  Network.Ethereum.ABI.Class
-- Copyright   :  Alexander Krupenkin 2016-2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  noportable
--
-- Ethereum ABI encoding base type classes.
--

module Network.Ethereum.ABI.Class (
    ABIType(..)
  , ABIPut(..)
  , ABIGet(..)
  , GenericABIPut(..)
  , GenericABIGet(..)
  ) where

import           Data.Proxy     (Proxy)
import           Data.Serialize (Get, Putter)
import           Generics.SOP   (Generic, Rep, from, to)

-- | A class for abi encoding datatype descriptions
class ABIType a where
    isDynamic :: Proxy a -> Bool

-- | A class for encoding datatypes to their abi encoding
--
-- If your compiler has support for the @DeriveGeneric@ and
-- @DefaultSignatures@ language extensions (@ghc >= 7.2.1@),
-- the 'abiPut' method will have default generic implementations.
--
-- To use this option, simply add a @deriving 'Generic'@ clause
-- to your datatype and declare a 'ABIPut' instance for it without
-- giving a definition for 'abiPut'.
--
class ABIType a => ABIPut a where
    abiPut :: Putter a

    default abiPut :: (Generic a, Rep a ~ rep, GenericABIPut rep) => Putter a
    abiPut = gAbiPut . from

-- | A class for encoding generically composed datatypes to their abi encoding
class GenericABIPut a where
    gAbiPut :: Putter a

-- | A class for decoding datatypes from their abi encoding
--
-- If your compiler has support for the @DeriveGeneric@ and
-- @DefaultSignatures@ language extensions (@ghc >= 7.2.1@),
-- the 'abiGet' method will have default generic implementations.
--
-- To use this option, simply add a @deriving 'Generic'@ clause
-- to your datatype and declare a 'ABIGet' instance for it without
-- giving a definition for 'abiGet'.
--
class ABIType a => ABIGet a where
    abiGet :: Get a

    default abiGet :: (Generic a, Rep a ~ rep, GenericABIGet rep) => Get a
    abiGet = to <$> gAbiGet

-- | A class for decoding generically composed datatypes from their abi encoding
class GenericABIGet a where
    gAbiGet :: Get a
