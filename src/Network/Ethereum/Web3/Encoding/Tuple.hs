{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module      :  Network.Ethereum.Web3.Encoding.Tuple
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- ABIEncoding tuple instances.
--
module Network.Ethereum.Web3.Encoding.Tuple (Singleton(..)) where

import Network.Ethereum.Web3.Encoding.Internal
import Network.Ethereum.Web3.Encoding.TupleTH
import Network.Ethereum.Web3.Encoding

-- | Singleton parameter instance
newtype Singleton a = Singleton { unSingleton :: a }

--instance (EncodingType a, ABIEncode a) => ABIEncode (Singleton a) where
--    toDataBuilder  = _serialize (1, []) . unSingleton
--
--instance (EncodingType a, ABIDecode a) => ABIDecode (Singleton a) where
--    fromDataParser = Singleton <$> (withParser sParser >>= dParser)
--      where withParser f = f undefined

-- | Tuple instances from 2 to 15 params
-- $(mkTupleInst 2)
-- $(mkTupleInst 3)
-- $(mkTupleInst 4)
-- $(mkTupleInst 5)
-- $(mkTupleInst 6)
-- $(mkTupleInst 7)
-- $(mkTupleInst 8)
-- $(mkTupleInst 9)
-- $(mkTupleInst 10)
-- $(mkTupleInst 11)
-- $(mkTupleInst 12)
-- $(mkTupleInst 13)
-- $(mkTupleInst 14)
-- $(mkTupleInst 15)
