{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      :  Network.Ethereum.ABI.Prim.Bool
-- Copyright   :  Alexander Krupenkin 2016-2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  noportable
--
-- Ethereum ABI boolean type.
--

module Network.Ethereum.ABI.Prim.Bool () where

import           Network.Ethereum.ABI.Class    (ABIGet (..), ABIPut (..),
                                                ABIType (..))
import           Network.Ethereum.ABI.Prim.Int (getWord256, putWord256)

instance ABIType Bool where
    isDynamic _ = False

instance ABIGet Bool where
    abiGet = toEnum . fromIntegral <$> getWord256

instance ABIPut Bool where
    abiPut = putWord256 . fromIntegral . fromEnum
