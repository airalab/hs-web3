{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Ipfs.Api.Api
-- Copyright   :  Alexander Krupenkin 2016-2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Multipart datatypes provider.
--

module Network.Ipfs.Api.Multipart where

import           Control.Monad
import           Data.Aeson     (FromJSON (..), Value(Object), (.:))
import qualified Data.Text      as TextS

data AddObj = AddObj
    { name        :: TextS.Text 
    , hash        :: TextS.Text
    , size        :: TextS.Text
    } deriving (Show, Eq)

instance FromJSON AddObj where
    parseJSON (Object o) =
        AddObj  <$> o .: "Name"
                <*> o .: "Hash"
                <*> o .: "Size"

    parseJSON _ = mzero
