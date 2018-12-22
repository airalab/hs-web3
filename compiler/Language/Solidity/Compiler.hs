{-# LANGUAGE CPP             #-}
#ifdef SOLIDITY_COMPILER
{-# LANGUAGE RecordWildCards #-}
#endif

-- |
-- Module      :  Language.Solidity.Compiler
-- Copyright   :  Alexander Krupenkin 2017
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Solidity compiler high-level bindings.
--

module Language.Solidity.Compiler where

#ifdef SOLIDITY_COMPILER

import           Data.ByteString                    (ByteString)
import           Data.Map                           (Map)
import           Data.Semigroup                     (Semigroup (..))
import qualified Language.Solidity.Compiler.Foreign as FFI
import           System.IO.Unsafe                   (unsafePerformIO)

-- | Input contract sources
data Sources = Sources
  { sourceMap    :: Map ByteString ByteString
  -- ^ Source code map from contract name in keys
  , libraries    :: Map ByteString ByteString
  -- ^ Library address map for linking
  , optimization :: Bool
  -- ^ Enable optimization?
  } deriving (Eq, Show)

instance Semigroup Sources where
    a <> b = Sources (sourceMap a `mappend` sourceMap b)
                     (libraries a `mappend` libraries b)
                     (optimization a || optimization b)

instance Monoid Sources where
    mappend = (<>)
    mempty = Sources mempty mempty False

type Compiled = Map ByteString (ByteString, ByteString)

-- | Compile Solidity contracts
compile :: Sources
        -- ^ Contract sources
        -> Either ByteString Compiled
        -- ^ Error string or compiled contracts ABI and hex
{-# NOINLINE compile #-}
compile Sources{..} = unsafePerformIO $
    FFI.compile sourceMap libraries optimization

#endif
