{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.Ethereum.Web3.Encoding.Vector where

import Control.Monad (replicateM)
import Data.Sized (Sized, toList, fromList, unsafeFromList)
import Data.Singletons (sing, Sing)
import Data.Proxy
import Network.Ethereum.Web3.Encoding (ABIEncode(..), ABIDecode(..))
import Network.Ethereum.Web3.Encoding.Internal (EncodingType(..))
import GHC.TypeLits

type Vector (n :: Nat) a = Sized [] n a

instance (ABIEncode a, KnownNat n) => ABIEncode (Vector n a) where
  toDataBuilder = foldMap toDataBuilder . toList

instance (ABIDecode a, KnownNat n) => ABIDecode (Vector n a) where
  fromDataParser = let len = natVal (Proxy :: Proxy n)
                   in unsafeFromList (sing :: Sing n) <$> replicateM (fromInteger len) fromDataParser
