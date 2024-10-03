-- |
-- Module      :  Network.Polkadot.Metadata.Type.Ast
-- Copyright   :  Aleksandr Krupenkin 2016-2024
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Runtime metadata type AST.
--

module Network.Polkadot.Metadata.Type.Ast where

import           Data.Text (Text)

-- | Qualified type parameter, e.g., <Vec<T> as SomeTrait>::SomeType.
type QSelf = (TypeAst, TypeAst)

-- | A segment of a path: an identifier and a set of argument types.
type PathSegment = (Text, Maybe [TypeAst])

-- | Simple Rust type AST is used for Metadata type representation.
data TypeAst
    = Slice !TypeAst
    -- ^ A variable-length slice ([T]).
    | Tuple ![TypeAst]
    -- ^ A tuple ((A, B, C, D,...)).
    | Array !TypeAst !Int
    -- ^ A fixed length array ([T; n]).
    | Path { qself    :: !(Maybe QSelf)
           -- ^ Two types of <Vec<T> as Trait>.
           , segments :: ![PathSegment]
           -- ^ A segment of a path: an identifier and a set of types.
           }
    -- ^ A "Path" is essentially Rust's notion of a name. It's represented as a sequence of identifiers,
    -- along with a bunch of supporting information. A path (module::module::...::Type), optionally "qualified",
    -- e.g., <Vec<T> as SomeTrait>::SomeType.
    deriving (Eq, Ord, Read, Show)
