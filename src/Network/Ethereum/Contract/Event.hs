{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}


-- |
-- Module      :  Network.Ethereum.Contract.Event
-- Copyright   :  Alexander Krupenkin 2016-2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- Ethereum contract event support.
--

module Network.Ethereum.Contract.Event
  ( EventAction(..)

  -- * Single event monitors
  , event
  , event'
  , eventMany'

  -- * MultiEventMonitors
  , MultiFilter(..)
  , minBlock
  , modifyMultiFilter
  , multiEvent
  , multiEvent'
  , multiEventMany'

  -- * ReExports
  , Handlers
  , Handler(..)
  , Rec(..)

  ) where

import           Network.Ethereum.Contract.Event.Common
import           Network.Ethereum.Contract.Event.MultiFilter
import           Network.Ethereum.Contract.Event.SingleFilter

--------------------------------------------------------------------------------

