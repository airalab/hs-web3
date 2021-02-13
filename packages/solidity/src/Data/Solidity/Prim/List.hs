{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :  Data.Solidity.Prim.List
-- Copyright   :  Aleksandr Krupenkin 2016-2021
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  noportable
--
-- Ethereum Abi dynamic and static size vectors based on linked lists.
--

module Data.Solidity.Prim.List
    (
    -- * Fixed size linked list
      ListN
    ) where

import           Basement.Nat           (NatWithinBound)
import           Basement.Sized.List    (ListN, toListN_, unListN)
import qualified Basement.Sized.List    as SL (mapM_, replicateM)
import           Control.Monad          (replicateM)
import           GHC.Exts               (IsList (..))
import           GHC.TypeLits           (KnownNat)

import           Data.Solidity.Abi      (AbiGet (..), AbiPut (..), AbiType (..))
import           Data.Solidity.Prim.Int (getWord256, putWord256)

instance AbiType [a] where
    isDynamic _ = True

instance AbiPut a => AbiPut [a] where
    abiPut l = do putWord256 $ fromIntegral (length l)
                  foldMap abiPut l

instance AbiGet a => AbiGet [a] where
    abiGet = do len <- fromIntegral <$> getWord256
                replicateM len abiGet

instance AbiType (ListN n a) where
    isDynamic _ = False

instance AbiPut a => AbiPut (ListN n a) where
    abiPut = SL.mapM_ abiPut

instance (NatWithinBound Int n, KnownNat n, AbiGet a) => AbiGet (ListN n a) where
    abiGet = SL.replicateM abiGet

instance (NatWithinBound Int n, KnownNat n) => IsList (ListN n a) where
    type Item (ListN n a) = a
    fromList = toListN_
    toList   = unListN
