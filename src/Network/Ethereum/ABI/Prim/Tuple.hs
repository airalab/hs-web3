{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}

-- |
-- Module      :  Network.Ethereum.ABI.Prim.Tuple
-- Copyright   :  Alexander Krupenkin 2016-2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  noportable
--
-- Tuple type abi encoding instances.
--

module Network.Ethereum.ABI.Prim.Tuple (
    Singleton(..)
  ) where

import           Data.Proxy                         (Proxy (..))
import           Generics.SOP                       (Generic)
import qualified GHC.Generics                       as GHC (Generic)

import           Network.Ethereum.ABI.Class         (ABIGet, ABIPut,
                                                     ABIType (..))
import           Network.Ethereum.ABI.Generic       ()
import           Network.Ethereum.ABI.Prim.Tuple.TH (tupleDecs)

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

$(fmap concat $ sequence $ map tupleDecs [2..20])
