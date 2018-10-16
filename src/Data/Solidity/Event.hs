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
-- Module      :  Data.Solidity.Event
-- Copyright   :  Alexander Krupenkin 2017-2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- This module is internal, the purpose is to define
-- helper classes and functions to assist in event decoding.
-- The user of this library should have no need to use
-- this directly in application code.
--

module Data.Solidity.Event
    (
      DecodeEvent(..)
    , IndexedEvent(..)
    ) where

import           Data.ByteArray               (ByteArrayAccess)
import           Data.Proxy                   (Proxy (..))
import           Generics.SOP                 (Generic, I (..), NP (..),
                                               NS (..), Rep, SOP (..), from, to)

import           Data.Solidity.Abi            (GenericAbiGet)
import           Data.Solidity.Abi.Codec      (decode')
import           Data.Solidity.Event.Internal
import           Network.Ethereum.Api.Types   (Change (..))

-- | Indexed event args come back in as a list of encoded values. 'ArrayParser'
-- | is used to decode these values so that they can be used to reconstruct the
-- | entire decoded event.
class ArrayParser a where
  arrayParser :: ByteArrayAccess ba
              => [ba]
              -> Either String a

instance ArrayParser (NP f '[]) where
  arrayParser _ = Right Nil

instance (ArrayParser (NP I as), Generic a, Rep a ~ rep, GenericAbiGet rep)
       => ArrayParser (NP I (a : as)) where
  arrayParser [] = Left "Empty"
  arrayParser (a : as) = do
    a' <- decode' a
    as' <- arrayParser as
    return $ I a' :* as'

instance ArrayParser (NP f as) => ArrayParser (SOP f '[as]) where
  arrayParser = fmap (SOP . Z) . arrayParser

genericArrayParser :: ( Generic a
                      , Rep a ~ rep
                      , ArrayParser rep
                      , ByteArrayAccess ba
                      )
                   => [ba]
                   -> Either String a
genericArrayParser = fmap to . arrayParser

--------------------------------------------------------------------------------
-- Event Parsing
--------------------------------------------------------------------------------

data Event i ni = Event i ni

-- | 'parseChange' decodes both the indexed and non-indexed event components.
parseChange :: ( Generic i
               , Rep i ~ irep
               , ArrayParser irep
               , Generic ni
               , Rep ni ~ nirep
               , GenericAbiGet nirep
               )
             => Change
             -> Bool
             -- ^ is anonymous event
             -> Either String (Event i ni)
parseChange change anonymous =
    Event <$> genericArrayParser topics <*> decode' data_
  where
    topics | anonymous = changeTopics change
           | otherwise = tail (changeTopics change)
    data_ = changeData change

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
  decodeEvent :: Change -> Either String e

instance ( IndexedEvent i ni e
         , Generic i
         , Rep i ~ SOP I '[hli]
         , Generic ni
         , Rep ni ~ SOP I '[hlni]
         , Generic e
         , Rep e ~ SOP I '[hle]
         , CombineChange i ni e
         , GenericAbiGet (SOP I '[hlni])
         , ArrayParser (SOP I '[hli])
         ) => DecodeEvent i ni e where
  decodeEvent change = do
      let anonymous = isAnonymous (Proxy :: Proxy e)
      (Event i ni :: Event i ni) <- parseChange change anonymous
      return $ combineChange i ni
