{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      :  Language.Solidity.Compiler
-- Copyright   :  Alexander Krupenkin 2017-2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Solidity compiler high-level bindings.
--

module Language.Solidity.Compiler where

import           Data.ByteString                    (ByteString)
import           Data.Map                           (Map)
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

instance Monoid Sources where
    mempty = Sources mempty mempty False
    a `mappend` b = Sources (sourceMap a `mappend` sourceMap b)
                            (libraries a `mappend` libraries b)
                            (optimization a || optimization b)

type Compiled = Map ByteString (ByteString, ByteString)

-- | Compile Solidity contracts
compile :: Sources
        -- ^ Contract sources
        -> Either ByteString Compiled
        -- ^ Error string or compiled contracts ABI and hex
{-# NOINLINE compile #-}
compile Sources{..} = unsafePerformIO $
    FFI.compile sourceMap libraries optimization
