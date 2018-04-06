{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Network.Ethereum.ABI.Prim.Tagged
-- Copyright   :  Alexander Krupenkin 2016-2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  noportable
--
-- Ethereum ABI UTF8-encoded tagged types.
--

module Network.Ethereum.ABI.Prim.Tagged (
    Tagged
  ) where

import           Data.Proxy                 (Proxy (..))
import           Data.Tagged                (Tagged (..))
import           Generics.SOP               (Generic)

import           Network.Ethereum.ABI.Class (ABIGet (..), ABIPut (..),
                                             ABIType (..))

instance ABIType a => ABIType (Tagged t a) where
    isDynamic _ = isDynamic (Proxy :: Proxy a)

instance ABIPut a => ABIPut (Tagged t a) where
    abiPut (Tagged a) = abiPut a

instance ABIGet a => ABIGet (Tagged t a) where
    abiGet = Tagged <$> abiGet

instance Generic a => Generic (Tagged t a)
