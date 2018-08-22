{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}

-- |
-- Module      :  Data.Solidity.Prim.Tuple
-- Copyright   :  Alexander Krupenkin 2016-2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  noportable
--
-- Tuple type abi encoding instances.
--

module Data.Solidity.Prim.Tuple (
    Singleton(..)
  ) where

import           Data.Proxy                  (Proxy (..))
import           Generics.SOP                (Generic)
import qualified GHC.Generics                as GHC (Generic)

import           Data.Solidity.Abi           (AbiGet, AbiPut, AbiType (..))
import           Data.Solidity.Abi.Generic   ()
import           Data.Solidity.Prim.Tuple.TH (tupleDecs)

-- | The type for one-tuples
newtype Singleton a = Singleton { unSingleton :: a }
  deriving GHC.Generic

deriving instance Eq a => Eq (Singleton a)
deriving instance Show a => Show (Singleton a)
instance Generic (Singleton a)

instance AbiType a => AbiType (Singleton a) where
    isDynamic _ = isDynamic (Proxy :: Proxy a)

instance AbiGet a => AbiGet (Singleton a)
instance AbiPut a => AbiPut (Singleton a)

$(fmap concat $ sequence $ map tupleDecs [2..20])
