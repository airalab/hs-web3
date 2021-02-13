{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Data.Solidity.Prim.Tagged
-- Copyright   :  Aleksandr Krupenkin 2016-2021
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  noportable
--
-- Ethereum Abi tagged types.
--

module Data.Solidity.Prim.Tagged
    (
    -- * The @Tagged@ type
      Tagged
    ) where

import           Data.Proxy        (Proxy (..))
import           Data.Tagged       (Tagged (..))
import           Generics.SOP      (Generic)

import           Data.Solidity.Abi (AbiGet (..), AbiPut (..), AbiType (..))

instance AbiType a => AbiType (Tagged t a) where
    isDynamic _ = isDynamic (Proxy :: Proxy a)

instance AbiPut a => AbiPut (Tagged t a) where
    abiPut (Tagged a) = abiPut a

instance AbiGet a => AbiGet (Tagged t a) where
    abiGet = Tagged <$> abiGet

instance Generic a => Generic (Tagged t a)
