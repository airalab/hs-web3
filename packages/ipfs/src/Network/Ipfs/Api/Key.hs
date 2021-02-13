{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

-- |
-- Module      :  Network.Ipfs.Api.Key
-- Copyright   :  Aleksandr Krupenkin 2016-2021
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unknown
--
-- Api calls with `key` prefix.
--

module Network.Ipfs.Api.Key where

import           Control.Monad.IO.Class         (MonadIO)
import           Data.Text                      (Text)

import           Network.Ipfs.Api.Internal      (_keyGen, _keyList, _keyRename,
                                                 _keyRm)
import           Network.Ipfs.Api.Internal.Call (call)
import           Network.Ipfs.Api.Types         (KeyDetailsObj, KeyObj,
                                                 KeyRenameObj)
import           Network.Ipfs.Client            (IpfsT)


-- | 'List all local keypairs.
list :: MonadIO m => IpfsT m KeyObj
list = call _keyList

-- | Create a new keypair.
gen :: MonadIO m => Text -> Text -> IpfsT m KeyDetailsObj
gen name = call . _keyGen name . Just

-- | Rename a keypair.
rename :: MonadIO m => Text -> Text -> IpfsT m KeyRenameObj
rename was = call . _keyRename was . Just

-- | Remove a keypair.
rm :: MonadIO m => Text -> IpfsT m KeyObj
rm = call . _keyRm

