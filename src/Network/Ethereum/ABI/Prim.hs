-- |
-- Module      :  Network.Ethereum.ABI.Prim
-- Copyright   :  Alexander Krupenkin 2016-2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  noportable
--
-- Ethereum ABI encoding primitive types.
--

module Network.Ethereum.ABI.Prim (
    Address
  , Bytes
  , BytesN
  , IntN
  , UIntN
  , ListN
  , Singleton(..)
  ) where

import           Network.Ethereum.ABI.Prim.Address (Address)
import           Network.Ethereum.ABI.Prim.Bool    ()
import           Network.Ethereum.ABI.Prim.Bytes   (Bytes, BytesN)
import           Network.Ethereum.ABI.Prim.Int     (IntN, UIntN)
import           Network.Ethereum.ABI.Prim.List    (ListN)
import           Network.Ethereum.ABI.Prim.String  ()
import           Network.Ethereum.ABI.Prim.Tagged  ()
import           Network.Ethereum.ABI.Prim.Tuple   (Singleton (..))
