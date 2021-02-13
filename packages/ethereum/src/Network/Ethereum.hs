-- |
-- Module      :  Network.Ethereum
-- Copyright   :  Aleksandr Krupenkin 2016-2021
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- Ethereum is a global, open-source platform for decentralized applications.
--

module Network.Ethereum
    (
    -- * Basic transaction sending
      module Account

    -- * Basic EVM event listening
    , EventAction(..)
    , event

    -- * Primitive Solidity data types
    , module Prim

    -- * Metric unit system
    , module Unit
    ) where

import           Data.Solidity.Prim              as Prim
import           Network.Ethereum.Account        as Account
import           Network.Ethereum.Contract.Event (EventAction (..), event)
import           Network.Ethereum.Unit           as Unit
