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
import           Data.Aeson                       (FromJSON, parseJSON, Object(..))
import           Data.Int
import           Data.ByteString.Lazy             (fromStrict, toStrict)
import qualified Data.ByteString.Lazy.Char8       as BC
import           Data.Proxy           
import qualified Data.Text                        as TextS
import qualified Data.Text.Encoding               as TextS
import           Data.Typeable            
import           Network.HTTP.Client              (newManager, defaultManagerSettings)
import qualified Network.HTTP.Media               as M ((//), (/:))
import           Servant.API
import           Servant.Client


type IpfsReturnType = TextS.Text

data DirContent = DirContent
    {   name          :: String 
    ,   hash          :: String
    ,   size          :: Int64
    ,   contentType   :: Int
    ,   target        :: String
    } deriving (Eq, Show)

instance FromJSON DirContent where
    parseJSON (Object o) =
        UserSummary <$> o .: "name"
                    <*> o .: "hash"
                    <*> o .: "size"
                    <*> o .: "contentType"
                    <*> o .: "target"

-- | Defining a content type same as PlainText without charset
data IpfsText deriving Typeable

instance Servant.API.Accept IpfsText where
    contentType _ = "text" M.// "plain"

-- | @left show . TextS.decodeUtf8' . toStrict@
instance MimeUnrender IpfsText TextS.Text where
    mimeUnrender _ = left show . TextS.decodeUtf8' . toStrict


type IpfsApi = "cat" :> Capture "cid" String :> Get '[IpfsText] IpfsReturnType
            :<|> "ls" :> Capture "cid" String :> Get '[JSON] [DirContent]

ipfsApi :: Proxy IpfsApi
ipfsApi =  Proxy

_cat :: String -> ClientM IpfsReturnType
_ls :: String -> ClientM [DirContent]

_cat :<|> _ls = client ipfsApi
