{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      :  Network.Ipfs.Api.Api
-- Copyright   :  Alexander Krupenkin 2016-2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Ipfs API provider.
--

module Network.Ipfs.Api.Api where

import           Control.Arrow                    (left)
import           Data.ByteString.Lazy             (fromStrict, toStrict)
import           Data.Proxy           
import           Data.Typeable            
import           Network.HTTP.Client              (newManager, defaultManagerSettings)
import           Servant.API
import           Servant.Client
import qualified Data.ByteString.Lazy.Char8       as BC
import qualified Data.Text                        as TextS
import qualified Data.Text.Encoding               as TextS
import qualified Network.HTTP.Media               as M ((//), (/:))


type IpfsReturnType = TextS.Text


-- | Defining a content type same as PlainText without charset
data IpfsText deriving Typeable

instance Servant.API.Accept IpfsText where
    contentType _ = "text" M.// "plain"

-- | @left show . TextS.decodeUtf8' . toStrict@
instance MimeUnrender IpfsText TextS.Text where
    mimeUnrender _ = left show . TextS.decodeUtf8' . toStrict


type IpfsApi = "cat" :> Capture "cid" String :> Get '[IpfsText] IpfsReturnType

ipfsApi :: Proxy IpfsApi
ipfsApi =  Proxy

_cat :: String -> ClientM IpfsReturnType
_cat = client ipfsApi