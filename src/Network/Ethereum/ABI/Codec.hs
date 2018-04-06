{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Network.Ethereum.ABI.Codec
-- Copyright   :  Alexander Krupenkin 2016-2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  noportable
--
-- Ethereum ABI encoding codec functions.
--

module Network.Ethereum.ABI.Codec (
    encode
  , decode
  , encode'
  , decode'
  ) where

import           Data.ByteArray               (ByteArray, ByteArrayAccess,
                                               convert)
import           Data.Serialize               (runGet, runPut)
import           Generics.SOP                 (Generic, Rep, from, to)

import           Network.Ethereum.ABI.Class   (ABIGet (..), ABIPut (..),
                                               GenericABIGet (..),
                                               GenericABIPut (..))
import           Network.Ethereum.ABI.Generic ()

-- | Encode datatype to Ethereum ABI-encoding
encode :: (ABIPut a, ByteArray ba)
       => a
       -> ba
{-# INLINE encode #-}
encode = convert . runPut . abiPut

-- | Generic driven version of 'encode'
encode' :: (Generic a,
            Rep a ~ rep,
            GenericABIPut rep,
            ByteArray ba)
        => a
        -> ba
{-# INLINE encode' #-}
encode' = convert . runPut . gAbiPut . from

-- | Decode datatype from Ethereum ABI-encoding
decode :: (ByteArrayAccess ba, ABIGet a)
       => ba
       -> Either String a
{-# INLINE decode #-}
decode = runGet abiGet . convert

-- | Generic driven version of 'decode'
decode' :: (Generic a,
            Rep a ~ rep,
            GenericABIGet rep,
            ByteArrayAccess ba)
        => ba
        -> Either String a
decode' = runGet (to <$> gAbiGet) . convert
