{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.Ethereum.Web3.Encoding.Vector where

import           Control.Monad                           (replicateM)
import           Data.Proxy
import           Data.Singletons                         (Sing, sing)
import           Data.Sized                              (Sized, fromList,
                                                          toList,
                                                          unsafeFromList)
import           GHC.TypeLits
import           Network.Ethereum.Web3.Encoding          (ABIDecode (..),
                                                          ABIEncode (..))
import           Network.Ethereum.Web3.Encoding.Internal (EncodingType (..))

type Vector (n :: Nat) a = Sized [] n a

instance (ABIEncode a, KnownNat n) => ABIEncode (Vector n a) where
  toDataBuilder = foldMap toDataBuilder . toList

instance (ABIDecode a, KnownNat n) => ABIDecode (Vector n a) where
  fromDataParser = let len = natVal (Proxy :: Proxy n)
                   in unsafeFromList (sing :: Sing n) <$> replicateM (fromInteger len) fromDataParser

instance (KnownNat n, EncodingType a) => EncodingType (Vector n a) where
  typeName = const $ typeName (Proxy :: Proxy a) ++ "[" ++ show (natVal (Proxy :: Proxy n)) ++ "]"
  isDynamic = const $ isDynamic (Proxy :: Proxy a)
