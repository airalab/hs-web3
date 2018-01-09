{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}


-- |
-- Module      :  Network.Ethereum.Web3.Encoding.Event
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- This module is internal, the purpose is to define helper classes and functions
-- to assist in event decoding. The user of this library should have no need to use
-- this directly in application code.
--

module Network.Ethereum.Web3.Encoding.Event(
    DecodeEvent(..)
  , ArrayParser(..)
  , IndexedEvent(..)
  , genericArrayParser
  ) where

import qualified Data.Text                                     as T
import           Generics.SOP
import qualified GHC.Generics                                  as GHC (Generic)

import           Network.Ethereum.Web3.Address                 (Address)
import           Network.Ethereum.Web3.Encoding                (ABIDecode,
                                                                fromData)
import           Network.Ethereum.Web3.Encoding.Event.Internal
import           Network.Ethereum.Web3.Encoding.Generic        (GenericABIDecode,
                                                                genericFromData)
import           Network.Ethereum.Web3.Encoding.Internal
import           Network.Ethereum.Web3.Types                   (Change (..))

-- | Indexed event args come back in as a list of encoded values. 'ArrayParser'
-- | is used to decode these values so that they can be used to reconstruct the
-- | entire decoded event.
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

-- | 'parseChange' decodes both the indexed and non-indexed event components.
parseChange :: ( Generic i
               , Show i
               , Rep i ~ irep
               , ArrayParser irep
               , Generic ni
               , Show ni
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
    strip0x hx = if T.take 2 hx == "0x" then T.drop 2 hx else hx
    topics = map strip0x $
      if isAnonymous
        then changeTopics change
        else tail $ changeTopics change
    data_ = strip0x $ changeData change

class IndexedEvent i ni e | e -> i ni where
  isAnonymous :: Proxy e -> Bool

-- | 'CombineChange' is a class which indicates that given event components of types 'i'
-- | and 'ni', we can construct an event of type 'e'. The functional dependency is valid
-- | becasue of how the template haskell generates the event types.
class CombineChange i ni e | e -> i ni where
  combineChange :: i -> ni -> e

instance ( Generic i
         , Rep i ~ irep
         , Generic ni
         , Rep ni ~ nirep
         , Generic e
         , Rep e ~ erep
         , HListRep irep hli
         , HListRep nirep hlni
         , MergeIndexedArguments hli hlni
         , MergeIndexedArguments' hli hlni ~ hle
         , HListRep erep hle
         , IndexedEvent i ni e
         ) => CombineChange i ni e where
  combineChange i ni =
    let hli = toHList . from $ i
        hlni = toHList . from $ ni
        hle = mergeIndexedArguments hli hlni
    in to . fromHList $ hle

class DecodeEvent i ni e | e -> i ni where
  decodeEvent :: Change -> Maybe e

instance ( IndexedEvent i ni e
         , Generic i
         , Rep i ~ SOP I '[hli]
         , Generic ni
         , Rep ni ~ SOP I '[hlni]
         , Generic e
         , Rep e ~ SOP I '[hle]
         , CombineChange i ni e
         , GenericABIDecode (SOP I '[hlni])
         , ArrayParser (SOP I '[hli])
         , Show i, Show ni
         ) => DecodeEvent i ni e where
  decodeEvent change = do
      let anonymous = isAnonymous (Proxy :: Proxy e)
      (Event i ni :: Event i ni) <- parseChange change anonymous
      return $ combineChange i ni

