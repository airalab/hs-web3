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
-- Ipfs Stream API provider.
--

module Network.Ipfs.Api.Stream where

import           Control.Monad
import           Data.Aeson
import           Data.Int
import qualified Data.ByteString.Lazy.Char8()
import           Data.Map()                    
import           Data.Proxy           
import qualified Data.Text                     as TextS
import           Network.HTTP.Client()
import           Servant.API
import           Servant.Client.Streaming      as S

data PingObj = PingObj
    { success  :: Bool 
    , text     :: TextS.Text
    , time     :: Int64
    } deriving (Show)

data ResponseObj = ResponseObj
    { addrs  :: [TextS.Text] 
    , id     :: TextS.Text
    } deriving (Show)

data DhtObj = DhtObj
    { extra       :: TextS.Text 
    , addid       :: TextS.Text
    , responses   :: Maybe [ResponseObj]
    , addrType    :: Int
    } deriving (Show)

instance FromJSON PingObj where
    parseJSON (Object o) =
        PingObj  <$> o .: "Success"
                 <*> o .: "Text"
                 <*> o .: "Time"
    
    parseJSON _ = mzero

instance FromJSON DhtObj where
    parseJSON (Object o) =
        DhtObj   <$> o .: "Extra"
                 <*> o .: "ID"
                 <*> o .: "Responses"
                 <*> o .: "Type"
    
    parseJSON _ = mzero

instance FromJSON ResponseObj where
    parseJSON (Object o) =
        ResponseObj  <$> o .: "Addrs"
                     <*> o .: "ID"
    
    parseJSON _ = mzero


type IpfsStreamApi = "ping" :> Capture "arg" TextS.Text :> StreamGet NewlineFraming JSON ( SourceIO PingObj )
                :<|> "dht" :> "findpeer" :> Capture "arg" TextS.Text :> StreamGet NewlineFraming JSON ( SourceIO DhtObj )

ipfsStreamApi :: Proxy IpfsStreamApi
ipfsStreamApi =  Proxy

_ping :: TextS.Text -> ClientM (SourceIO PingObj)
_dhtFindPeer :: TextS.Text -> ClientM (SourceIO DhtObj)

_ping :<|> _dhtFindPeer = client ipfsStreamApi