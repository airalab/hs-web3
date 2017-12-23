{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Network.Ethereum.Web3.Encoding.Event where

import Data.Kind
import Data.Tagged (Tagged(..))
import qualified Data.Text as T
import Data.Proxy (Proxy(..))
import qualified GHC.Generics as GHC (Generic)
import Generics.SOP
import GHC.TypeLits (CmpNat, Nat)


class ArrayParser a where
  arrayParser :: [T.Text] -> Maybe a

--------------------------------------------------------------------------------
-- Event Parsing Internals
--------------------------------------------------------------------------------

data HList :: [*] -> * where
  HNil :: HList '[]
  (:<) :: a -> HList as -> HList (a : as)

infixr 0 :<

-- | Generic representation to HList representation
class HListRep a xs | a -> xs where
  toHList :: a -> HList xs
  fromHList :: HList xs -> a

instance HListRep (NP I '[]) '[] where
  toHList _ = HNil
  fromHList _ = Nil

instance HListRep (NP I as) as => HListRep (NP I (a:as)) (a:as) where
  toHList (I a :* rest) = a :< toHList rest
  fromHList (a :< rest) = I a :* fromHList rest

instance HListRep (NP f as') as => HListRep (NS (NP f) '[as']) as where
  toHList (Z rep) = toHList rep
  fromHList = Z . fromHList

instance HListRep (NS (NP f) as') as => HListRep (SOP f as') as where
  toHList (SOP rep) = toHList rep
  fromHList = SOP . fromHList

-- | Sort a Tagged HList
class Sort (xs :: [*]) where
  type Sort' xs :: [*]
  sort :: HList xs -> HList (Sort' xs)

instance Sort '[] where
  type Sort' '[] = '[]
  sort HNil = HNil

instance (Sort xs, Insert x (Sort' xs)) => Sort (x : xs) where
  type Sort' (x : xs) = Insert' x (Sort' xs)
  sort (x :< xs) = insert x (sort xs)

class Insert (x :: *) (xs :: [*]) where
  type Insert' x xs :: [*]
  insert :: x -> HList xs -> HList (Insert' x xs)

instance Insert x '[] where
  type Insert' x '[] = '[x]
  insert x HNil = x :< HNil

instance InsertCmp (CmpNat n m) (Tagged n x) (Tagged m y) ys => Insert (Tagged n x) (Tagged m y : ys) where
  type Insert' (Tagged n x) (Tagged m y : ys) = InsertCmp' (CmpNat n m) (Tagged n x) (Tagged m y) ys
  insert (x :: Tagged n x) ((y :: Tagged m y) :< ys) = insertCmp (Proxy :: Proxy (CmpNat n m)) x y ys

class InsertCmp (b :: Ordering) (x :: *) (y :: *) (ys :: [*]) where
  type InsertCmp' b x y ys :: [*]
  insertCmp :: Proxy (b :: Ordering) -> x -> y -> HList ys -> HList (InsertCmp' b x y ys)

instance InsertCmp 'LT x y ys where
  type InsertCmp' 'LT x y ys = x : (y : ys)
  insertCmp _ x y ys = x :< y :< ys

instance Insert x ys => InsertCmp 'GT x y ys where
  type InsertCmp' 'GT x y ys = y : Insert' x ys
  insertCmp _ x y ys = y :< insert x ys

-- | Unwrap all the Tagged items in an HList
class UnTag t ut | t -> ut where
  unTag :: HList t -> HList ut

instance UnTag '[] '[] where
  unTag a = a

instance UnTag ts uts => UnTag (Tagged n a : ts) (a : uts) where
  unTag (Tagged a :< ts) = a :< unTag ts

class HListMerge (as :: [*]) (bs :: [*]) where
  type Concat as bs :: [*]
  mergeHList :: HList as -> HList bs -> HList (Concat as bs)

instance HListMerge '[] bs where
  type Concat '[] bs = bs
  mergeHList _ bs = bs

instance HListMerge as bs => HListMerge (a : as) bs where
  type Concat (a : as) bs = a : Concat as bs
  mergeHList (a :< as) bs = a :< mergeHList as bs
