{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}


-- |
-- Module      :  Network.Ethereum.ABI.Prim.Singleton
-- Copyright   :  Alexander Krupenkin 2016-2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  noportable
--
-- One-tuple type and its encoding instances.
--

module Network.Ethereum.ABI.Prim.Singleton (
    Singleton(..)
  ) where

import           Data.Proxy                   (Proxy (..))
import           Generics.SOP                 (Generic)
import qualified GHC.Generics                 as GHC (Generic)

import           Network.Ethereum.ABI.Class   (ABIGet, ABIPut, ABIType (..))
import           Network.Ethereum.ABI.Generic ()

-- | The type for one-tuples
newtype Singleton a = Singleton { unSingleton :: a }
  deriving GHC.Generic

deriving instance Eq a => Eq (Singleton a)
deriving instance Show a => Show (Singleton a)
instance Generic (Singleton a)

instance ABIType a => ABIType (Singleton a) where
    isDynamic _ = isDynamic (Proxy :: Proxy a)

instance ABIGet a => ABIGet (Singleton a)
instance ABIPut a => ABIPut (Singleton a)
