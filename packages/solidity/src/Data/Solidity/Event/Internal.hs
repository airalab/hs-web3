{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

-- |
-- Module      :  Data.Solidity.Event.Internal
-- Copyright   :  Aleksandr Krupenkin 2016-2024
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- This module is internal, the purpose is to define helper classes and types
-- to assist in event decoding. The user of this library should have no need to use
-- this directly in application code.
--

module Data.Solidity.Event.Internal where

import           Data.Kind    (Type)
import           Data.Proxy   (Proxy (..))
import           Data.Tagged  (Tagged (..))
import           Generics.SOP (I (..), NP (..), NS (..), SOP (..))
import           GHC.TypeLits (CmpNat)

data HList :: [Type] -> Type where
  HNil :: HList '[]
  (:<) :: a -> HList as -> HList (a : as)

infixr 0 :<

-- | Generic representation to HList representation
class HListRep a xs | a -> xs, a -> xs where
  toHList :: a -> HList xs
  fromHList :: HList xs -> a

instance HListRep (NP I '[]) '[] where
  toHList _ = HNil
  fromHList _ = Nil

instance HListRep (NP I as) as => HListRep (NP I (a:as)) (a:as) where
  toHList (I a :* rest) = a :< toHList rest
  fromHList (a :< rest) = I a :* fromHList rest

instance HListRep (NP f as') as => HListRep (SOP f '[as']) as where
  toHList (SOP (Z rep)) = toHList rep
  toHList _             = error "Impossible branch"
  fromHList = SOP . Z . fromHList

-- | Sort a Tagged HList
class Sort (xs :: [Type]) where
  type Sort' xs :: [Type]
  sort :: HList xs -> HList (Sort' xs)

instance Sort '[] where
  type Sort' '[] = '[]
  sort HNil = HNil

instance (Sort xs, Insert x (Sort' xs)) => Sort (x : xs) where
  type Sort' (x : xs) = Insert' x (Sort' xs)
  sort (x :< xs) = insert x (sort xs)

class Insert (x :: Type) (xs :: [Type]) where
  type Insert' x xs :: [Type]
  insert :: x -> HList xs -> HList (Insert' x xs)

instance Insert x '[] where
  type Insert' x '[] = '[x]
  insert x HNil = x :< HNil

instance InsertCmp (CmpNat n m) (Tagged n x) (Tagged m y) ys => Insert (Tagged n x) (Tagged m y : ys) where
  type Insert' (Tagged n x) (Tagged m y : ys) = InsertCmp' (CmpNat n m) (Tagged n x) (Tagged m y) ys
  insert (x :: Tagged n x) ((y :: Tagged m y) :< ys) = insertCmp (Proxy :: Proxy (CmpNat n m)) x y ys

class InsertCmp (b :: Ordering) (x :: Type) (y :: Type) (ys :: [Type]) where
  type InsertCmp' b x y ys :: [Type]
  insertCmp :: Proxy (b :: Ordering) -> x -> y -> HList ys -> HList (InsertCmp' b x y ys)

instance InsertCmp 'LT x y ys where
  type InsertCmp' 'LT x y ys = x : (y : ys)
  insertCmp _ x y ys = x :< y :< ys

instance Insert x ys => InsertCmp 'GT x y ys where
  type InsertCmp' 'GT x y ys = y : Insert' x ys
  insertCmp _ x y ys = y :< insert x ys

-- | Unwrap all the Tagged items in an HList
class UnTag t where
  type UnTag' t :: [Type]
  unTag :: HList t -> HList (UnTag' t)

instance UnTag '[] where
  type UnTag' '[] = '[]
  unTag a = a

instance UnTag ts => UnTag (Tagged n a : ts) where
  type UnTag' (Tagged n a : ts) = a : UnTag' ts
  unTag (Tagged a :< ts) = a :< unTag ts

class HListMerge (as :: [Type]) (bs :: [Type]) where
  type Concat as bs :: [Type]
  mergeHList :: HList as -> HList bs -> HList (Concat as bs)

instance HListMerge '[] bs where
  type Concat '[] bs = bs
  mergeHList _ bs = bs

instance HListMerge as bs => HListMerge (a : as) bs where
  type Concat (a : as) bs = a : Concat as bs
  mergeHList (a :< as) bs = a :< mergeHList as bs

class HListMergeSort as bs where
  type MergeSort' as bs :: [Type]
  mergeSortHList :: HList as -> HList bs -> HList (MergeSort' as bs)

instance (HListMerge as bs, Concat as bs ~ cs, Sort cs, Sort' cs ~ cs') => HListMergeSort as bs where
  type MergeSort' as bs = Sort' (Concat as bs)
  mergeSortHList as bs = sort $ (mergeHList as bs :: HList cs)

class MergeIndexedArguments as bs where
  type MergeIndexedArguments' as bs :: [Type]
  mergeIndexedArguments :: HList as -> HList bs -> HList (MergeIndexedArguments' as bs)

instance (HListMergeSort as bs, MergeSort' as bs ~ cs, UnTag cs, UnTag cs' ~ ds) => MergeIndexedArguments as bs where
  type MergeIndexedArguments' as bs = (UnTag' (MergeSort' as bs))
  mergeIndexedArguments as bs = unTag $ mergeSortHList as bs
