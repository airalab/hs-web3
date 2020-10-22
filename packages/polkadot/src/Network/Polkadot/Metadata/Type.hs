{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Polkadot.Metadata.Type
-- Copyright   :  Aleksandr Krupenkin 2016-2020
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Runtime type names metadata encoding.
--

module Network.Polkadot.Metadata.Type where

import           Codec.Scale.Class (Decode (..), Encode (..))
import           Codec.Scale.Core  ()
import           Data.Aeson        (FromJSON (..), ToJSON (..), Value (String))
import           Data.Text         (Text, replace, strip)

-- | This is a extended version of String, specifically to handle types. Here we rely fully
-- on what string provides us, however we also adjust the types received from the runtime,
-- i.e. we remove the `T::` prefixes found in some types for consistency across implementation.
newtype Type = Type { unType :: Text }
    deriving (Eq, Ord, Show)

instance Decode Type where
    get = Type <$> get

instance Encode Type where
    put = put . unType

instance FromJSON Type where
    parseJSON (String s) = return $ Type (sanitize s)
    parseJSON _          = fail "Type name should be a string"

instance ToJSON Type where
    toJSON = toJSON . unType

-- | Runtime type name sanitizing.
sanitize :: Text -> Text
sanitize = strip
    -- alias <T::InherentOfflineReport as InherentOfflineReport>::Inherent -> InherentOfflineReport
    . replace "<T::InherentOfflineReport as InherentOfflineReport>::Inherent" "InherentOfflineReport"
    -- alias String -> Text (compat with jsonrpc methods)
    . replace "String" "Text"
    -- alias Vec<u8> -> Bytes
    . replace "Vec<u8>" "Bytes"
    . replace "&[u8]" "Bytes"
    -- alias RawAddress -> Address
    . replace "RawAddress" "Address"
    -- lookups, mapped to Address/AccountId as appropriate in runtime
    . replace "Lookup::Source" "LookupSource"
    . replace "Lookup::Target" "LookupTarget"
    -- HACK duplication between contracts & primitives, however contracts prefixed with exec
    . replace "exec::StorageKey" "ContractStorageKey"
    -- <T::Balance as HasCompact>
    . cleanupCompact
    -- remove all the trait prefixes
    . removeTraits
    -- remove PairOf<T> -> (T, T)
    . removePairOf
    -- remove boxing, `Box<Proposal>` -> `Proposal`
    . removeWrap "Box"
    -- remove generics, `MisbehaviorReport<Hash, BlockNumber>` -> `MisbehaviorReport`
    . removeGenerics
    -- flattens tuples with one value, `(AccountId)` -> `AccountId
    . flattenSingleTuple
    -- converts ::Type to Type, <T as Trait<I>>::Proposal -> Proposal
    . removeColons

alias :: Text -> Text -> Text -> Text
alias = undefined

cleanupCompact :: Text -> Text
cleanupCompact = id

removeTraits :: Text -> Text
removeTraits = id

removePairOf :: Text -> Text
removePairOf = id

removeWrap :: Text -> Text -> Text
removeWrap _ = id

removeGenerics :: Text -> Text
removeGenerics = id

flattenSingleTuple :: Text -> Text
flattenSingleTuple = id

removeColons :: Text -> Text
removeColons = id
