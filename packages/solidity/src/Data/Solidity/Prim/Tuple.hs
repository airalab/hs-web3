{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Module      :  Data.Solidity.Prim.Tuple
-- Copyright   :  Aleksandr Krupenkin 2016-2024
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  noportable
--
-- Tuple type abi encoding instances.
--

module Data.Solidity.Prim.Tuple where

import           Data.Proxy                  (Proxy (..))
#if MIN_VERSION_OneTuple(0,3,0)
import           Data.Tuple.Solo             (Solo (..))
#else
import           Data.Tuple.OneTuple         (OneTuple (..))
#endif
import           Generics.SOP                (Generic)
#if MIN_VERSION_base(4,15,0)
#else
import qualified GHC.Generics                as GHC (Generic)
#endif

import           Data.Solidity.Abi           (AbiGet, AbiPut, AbiType (..))
import           Data.Solidity.Abi.Generic   ()
import           Data.Solidity.Prim.Tuple.TH (tupleDecs)

#if MIN_VERSION_OneTuple(0,3,0)
instance Generic (Solo a)
instance AbiType a => AbiType (Solo a) where
    isDynamic _ = isDynamic (Proxy :: Proxy a)

instance AbiGet a => AbiGet (Solo a)
instance AbiPut a => AbiPut (Solo a)
#else
#if MIN_VERSION_base(4,15,0)
#else
deriving instance GHC.Generic (OneTuple a)
#endif
instance Generic (OneTuple a)

instance AbiType a => AbiType (OneTuple a) where
    isDynamic _ = isDynamic (Proxy :: Proxy a)

instance AbiGet a => AbiGet (OneTuple a)
instance AbiPut a => AbiPut (OneTuple a)
#endif
$(concat <$> mapM tupleDecs [2..20])
