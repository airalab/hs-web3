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

module Network.Ethereum.Web3.Encoding.Event where

import Data.Kind
import Data.Tagged (Tagged(..))
import qualified Data.Text as T
import Data.Proxy (Proxy(..))
import qualified GHC.Generics as GHC (Generic)
import Generics.SOP
import GHC.TypeLits (CmpNat, Nat)

import Network.Ethereum.Web3.Encoding (ABIDecode, fromData)
import Network.Ethereum.Web3.Encoding.Internal
import Network.Ethereum.Web3.Types (Change(..))
import Network.Ethereum.Web3.Address (Address)
import Network.Ethereum.Web3.Encoding.Generic (GenericABIDecode, genericFromData)

class ArrayParser a where
  arrayParser :: [T.Text] -> Maybe a

instance ArrayParser (NP f '[]) where
  arrayParser _ = Just Nil

instance (ArrayParser (NP I as), ABIDecode a) => ArrayParser (NP I (a : as)) where
  arrayParser [] = Nothing
  arrayParser (a : as) = do
    a' <- fromData a
    as' <- arrayParser as
    return $ I a' :* as'

instance ArrayParser (NP f as) => ArrayParser (SOP f '[as]) where
  arrayParser = fmap (SOP . Z) . arrayParser

genericArrayParser :: ( Generic a
                      , Rep a ~ rep
                      , ArrayParser rep
                      )
                    => [T.Text]
                    -> Maybe a
genericArrayParser = fmap to . arrayParser


--------------------------------------------------------------------------------
-- Event Parsing
--------------------------------------------------------------------------------

data Event i ni = Event i ni

parseChange :: ( Generic i
               , Rep i ~ irep
               , ArrayParser irep
               , Generic ni
               , Rep ni ~ nirep
               , GenericABIDecode nirep
               )
             => Change
             -> Bool -- is anonymous event
             -> Maybe (Event i ni)
parseChange change isAnonymous = do
    i <- genericArrayParser topics
    ni <- genericFromData data_
    return $ Event i ni
  where
    topics = if isAnonymous
               then changeTopics change
               else tail $ changeTopics change
    data_ = changeData change

combineChange :: ( Generic i
                 , Rep i ~ irep
                 , Generic ni
                 , Rep ni ~ nirep
                 , Generic e
                 , Rep e ~ erep
                 , HListRep irep hli
                 , HListRep nirep hlni
                 , HListMerge hli hlni
                 , Concat hli hlni ~ all
                 , Sort all
                 , Sort' all ~ all'
                 , UnTag all'
                 , UnTag' all' ~ hle
                 , HListRep erep hle
                 )
              => Event i ni
              -> e
combineChange (Event i ni) =
  let hli = toHList . from $ i
      hlni = toHList . from $ ni
      hle = unTag . sort $ mergeHList hli hlni
  in to . fromHList $ hle

class IndexedEvent e  where
  type IndexedArgs e :: *
  type NonIndexedArgs e :: *
  isAnonymous :: Proxy e -> Bool

-- example

data TransferIndexed = TransferIndexed (Tagged 1 Address) (Tagged 2 Address) deriving (GHC.Generic)

instance Generic TransferIndexed

data TransferNonIndexed = TransferNonIndexed (Tagged 3 Integer) deriving (GHC.Generic)

instance Generic TransferNonIndexed

data Transfer = Transfer Address Address Integer deriving (GHC.Generic)

instance Generic Transfer

instance IndexedEvent Transfer where
  type IndexedArgs Transfer = TransferIndexed
  type NonIndexedArgs Transfer = TransferNonIndexed
  isAnonymous = const False

transferInstance :: Transfer
transferInstance = combineChange (undefined :: Event TransferIndexed TransferNonIndexed)

decoded :: Maybe (Event TransferIndexed TransferNonIndexed)
decoded = parseChange undefined undefined

--

--decodeEvent :: ( IndexedEvent e
--               , IndexedArgs e ~ i
--               , Generic i
--               , Rep i ~ SOP I '[hli]
--               , NonIndexedArgs e ~ ni
--               , Generic ni
--               , Rep ni ~ SOP I '[hlni]
--               , Generic e
--               , Rep e ~ erep
--               , HListRep (Rep i) hli
--               , HListRep (Rep ni) hlni
--               , HListMerge hli hlni
--               , Concat hli hlni ~ all
--               , Sort all
--               , Sort' all ~ all'
--               , UnTag all'
--               , UnTag' all' ~ hle
--               , HListRep erep hle
--               , GenericABIDecode (SOP I '[hlni])
--               , ArrayParser (SOP I '[hli])
--               )
--             => Proxy e
--             -> Change
--             -> Maybe e
--decodeEvent pe change = do
--  let anonymous = isAnonymous pe
--  (event :: Event () ni) <- undefined
--  return $ combineChange event

--------------------------------------------------------------------------------
-- Event Parsing Internals
--------------------------------------------------------------------------------

data HList :: [*] -> * where
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
  fromHList = SOP . Z . fromHList

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
class UnTag t where
  type UnTag' t :: [*]
  unTag :: HList t -> HList (UnTag' t)

instance UnTag '[] where
  type UnTag' '[] = '[]
  unTag a = a

instance UnTag ts => UnTag (Tagged n a : ts) where
  type UnTag' (Tagged n a : as) = a : UnTag' as
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
