-- |
-- Module      :  Network.Polkadot
-- Copyright   :  Aleksandr Krupenkin 2016-2020
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- A scalable, interoperable & secure network protocol for the next web.
--

module Network.Polkadot
  (
  -- * Query blockchain storage.
    query
  , Argument(..)
  -- * Base types and codecs.
  , module Scale
  , module Primitives
  ) where

import           Codec.Scale                  as Scale
import           Network.Polkadot.Primitives  as Primitives
import           Network.Polkadot.Query       (query)
import           Network.Polkadot.Storage.Key (Argument (..))
