-- |
-- Module      :  Data.Solidity.Prim
-- Copyright   :  Alexander Krupenkin 2016-2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  noportable
--
-- Solidity primitive data types.
--

module Data.Solidity.Prim (
    Address
  , Bytes
  , BytesN
  , IntN
  , UIntN
  , ListN
  , Singleton(..)
  ) where

import           Data.Solidity.Prim.Address (Address)
import           Data.Solidity.Prim.Bool    ()
import           Data.Solidity.Prim.Bytes   (Bytes, BytesN)
import           Data.Solidity.Prim.Int     (IntN, UIntN)
import           Data.Solidity.Prim.List    (ListN)
import           Data.Solidity.Prim.String  ()
import           Data.Solidity.Prim.Tagged  ()
import           Data.Solidity.Prim.Tuple   (Singleton (..))
