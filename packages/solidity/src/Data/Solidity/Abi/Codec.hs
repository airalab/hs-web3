{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Data.Solidity.Abi.Codec
-- Copyright   :  Aleksandr Krupenkin 2016-2021
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  noportable
--
-- Solidity contract ABI encoding functions.
--

module Data.Solidity.Abi.Codec
    (
    -- * @AbiPut@/@AbiGet@ type class encoding
      encode
    , decode

    -- * Generic encoding
    , encode'
    , decode'

    -- * Generic type re-export
    , Generic
    ) where

import           Data.ByteArray            (ByteArray, ByteArrayAccess, convert)
import           Data.Serialize            (runGet, runPut)
import           Generics.SOP              (Generic, Rep, from, to)

import           Data.Solidity.Abi         (AbiGet (..), AbiPut (..),
                                            GenericAbiGet (..),
                                            GenericAbiPut (..))
import           Data.Solidity.Abi.Generic ()

-- | Encode datatype to Ethereum Abi-encoding
encode :: (AbiPut a, ByteArray ba)
       => a
       -> ba
{-# INLINE encode #-}
encode = convert . runPut . abiPut

-- | Generic driven version of 'encode'
encode' :: (Generic a,
            Rep a ~ rep,
            GenericAbiPut rep,
            ByteArray ba)
        => a
        -> ba
{-# INLINE encode' #-}
encode' = convert . runPut . gAbiPut . from

-- | Decode datatype from Ethereum Abi-encoding
decode :: (ByteArrayAccess ba, AbiGet a)
       => ba
       -> Either String a
{-# INLINE decode #-}
decode = runGet abiGet . convert

-- | Generic driven version of 'decode'
decode' :: (Generic a,
            Rep a ~ rep,
            GenericAbiGet rep,
            ByteArrayAccess ba)
        => ba
        -> Either String a
{-# INLINE decode' #-}
decode' = runGet (to <$> gAbiGet) . convert
