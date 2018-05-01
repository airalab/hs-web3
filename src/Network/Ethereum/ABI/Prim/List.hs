{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :  Network.Ethereum.ABI.Prim.List
-- Copyright   :  Alexander Krupenkin 2016-2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  noportable
--
-- Ethereum ABI dynamic and static size vectors based on linked lists.
--

module Network.Ethereum.ABI.Prim.List (
    ListN
  ) where

import           Basement.Nat                  (NatWithinBound)
import           Basement.Sized.List           (ListN, toListN_, unListN)
import qualified Basement.Sized.List           as SL (mapM_, replicateM)
import           Control.Monad                 (replicateM)
import           GHC.Exts                      (IsList (..))
import           GHC.TypeLits                  (KnownNat)

import           Network.Ethereum.ABI.Class    (ABIGet (..), ABIPut (..),
                                                ABIType (..))
import           Network.Ethereum.ABI.Prim.Int (getWord256, putWord256)

instance ABIType [a] where
    isDynamic _ = True

instance ABIPut a => ABIPut [a] where
    abiPut l = do putWord256 $ fromIntegral (length l)
                  foldMap abiPut l

instance ABIGet a => ABIGet [a] where
    abiGet = do len <- fromIntegral <$> getWord256
                replicateM len abiGet

instance ABIType (ListN n a) where
    isDynamic _ = False

instance ABIPut a => ABIPut (ListN n a) where
    abiPut = SL.mapM_ abiPut

instance (NatWithinBound Int n, KnownNat n, ABIGet a) => ABIGet (ListN n a) where
    abiGet = SL.replicateM abiGet

instance (NatWithinBound Int n, KnownNat n) => IsList (ListN n a) where
    type Item (ListN n a) = a
    fromList = toListN_
    toList   = unListN
