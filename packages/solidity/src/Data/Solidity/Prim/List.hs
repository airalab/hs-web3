{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}

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
import qualified Basement.Sized.List    as SL (init, map, mapM, mapM_, replicateM, scanl')
import           Basement.Types.Word256 (Word256)
import           Control.Monad          (replicateM, forM)
import qualified Data.ByteString        as B
import           Data.List              (scanl')
import           Data.Proxy             (Proxy (..))
import           Data.Serialize.Put     (runPut, putByteString)
import           Data.Serialize.Get     (skip, lookAhead)
import           GHC.Exts               (IsList (..))
import           GHC.TypeLits           (KnownNat, natVal, type (+), type (<=))

import           Data.Solidity.Abi      (AbiGet (..), AbiPut (..), AbiType (..))
import           Data.Solidity.Prim.Int (getWord256, putWord256)

instance AbiType [a] where
    isDynamic _ = True

instance AbiPut a => AbiPut [a] where
    abiPut l = do putWord256 $ fromIntegral (length l)
                  if isDynamic (Proxy :: Proxy a) then do
                      let encs = map (runPut . abiPut) l
                          lengths = map ((fromIntegral :: Int -> Word256) . B.length) encs
                          offsets = init $ scanl' (+) (fromIntegral (0x20 * length l)) lengths
                      mapM_ putWord256 offsets
                      mapM_ putByteString encs
                    else
                      foldMap abiPut l

instance AbiGet a => AbiGet [a] where
    abiGet = do len <- fromIntegral <$> getWord256
                if isDynamic (Proxy :: Proxy a) then do
                    offsets <- replicateM len getWord256
                    let currentOffset = 0x20 * len
                    forM offsets $ \dataOffset -> lookAhead $ do
                        skip (fromIntegral dataOffset - currentOffset)
                        abiGet
                  else
                    replicateM len abiGet

instance (AbiType a, KnownNat n) => AbiType (ListN n a) where
    isDynamic _ = natVal (Proxy :: Proxy n) > 0 && isDynamic (Proxy :: Proxy a)

instance (AbiPut a, KnownNat n, 1 <= n+1) => AbiPut (ListN n a) where
    abiPut l = if isDynamic (Proxy :: Proxy a) then do
                   let encs = SL.map (runPut . abiPut) l
                       lengths = SL.map ((fromIntegral :: Int -> Word256) . B.length) encs
                       len = fromInteger (natVal (Proxy :: Proxy n))
                       offsets = SL.init $ SL.scanl' (+) (fromIntegral (0x20 * len)) lengths
                   SL.mapM_ putWord256 offsets
                   SL.mapM_ putByteString encs
               else
                   SL.mapM_ abiPut l

instance (NatWithinBound Int n, KnownNat n, AbiGet a) => AbiGet (ListN n a) where
    abiGet = do let len = fromInteger (natVal (Proxy :: Proxy n))
                if isDynamic (Proxy :: Proxy a) then do
                    offsets <- SL.replicateM getWord256
                    let currentOffset = 0x20 * len
                    flip SL.mapM offsets $ \dataOffset -> lookAhead $ do
                        skip (fromIntegral dataOffset - currentOffset)
                        abiGet
                  else
                    SL.replicateM abiGet

instance (NatWithinBound Int n, KnownNat n) => IsList (ListN n a) where
    type Item (ListN n a) = a
    fromList = toListN_
    toList   = unListN
