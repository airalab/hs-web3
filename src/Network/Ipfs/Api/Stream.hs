{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      :  Network.Ipfs.Api.Stream
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
import           Data.Proxy           
import qualified Data.Text                     as TextS
import           Network.HTTP.Client()
import           Servant.API
import           Servant.Client.Streaming      as S

import           Network.Ipfs.Api.Api         (IpfsText)

type LogReturnType = TextS.Text

data PingObj = PingObj
    { success  :: Bool 
    , text     :: TextS.Text
    , time     :: Int64
    } deriving (Show, Eq)

data ResponseObj = ResponseObj
    { addrs  :: Maybe [TextS.Text] 
    , id     :: TextS.Text
    } deriving (Show, Eq)

data DhtObj = DhtObj
    { extra      :: TextS.Text 
    , addrid     :: TextS.Text
    , responses  :: Maybe [ResponseObj]
    , addrType   :: Int
    } deriving (Show, Eq)

data RepoKeyObj = RepoKeyObj { repoSlash :: TextS.Text } deriving (Show, Eq)  

data RepoGcObj = RepoGcObj { repoKey :: RepoKeyObj } deriving (Show, Eq)  

data RepoVerifyObj = RepoVerifyObj
    { msg       :: TextS.Text 
    , progress  :: Int
    } deriving (Show, Eq)

data RefsObj = RefsObj
    { error :: TextS.Text
    , ref   :: TextS.Text 
    } deriving (Show, Eq)

data PubsubSubObj = PubsubSubObj
    {  mssgdata  :: TextS.Text
    ,  from      :: TextS.Text
    ,  seqno     :: TextS.Text
    ,  topicIDs  :: [TextS.Text]
    }  deriving (Show, Eq)

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

instance FromJSON RepoKeyObj where
    parseJSON (Object o) =
        RepoKeyObj  <$> o .: "/"
    
    parseJSON _ = mzero

instance FromJSON RepoGcObj where
    parseJSON (Object o) =
        RepoGcObj  <$> o .: "Key"
    
    parseJSON _ = mzero

instance FromJSON RepoVerifyObj where
    parseJSON (Object o) =
        RepoVerifyObj <$> o .: "Msg"
                      <*> o .: "Progress"
    
    parseJSON _ = mzero

instance FromJSON RefsObj where
    parseJSON (Object o) =
        RefsObj  <$> o .: "Err"
                 <*> o .: "Ref"

    parseJSON _ = mzero

instance FromJSON PubsubSubObj where
    parseJSON (Object o) =
        PubsubSubObj   <$> o .: "data"
                       <*> o .: "from"
                       <*> o .: "seqno"
                       <*> o .: "topicIDs"
    
    parseJSON _ = mzero

type IpfsStreamApi = "ping" :> Capture "arg" TextS.Text :> StreamGet NewlineFraming JSON ( SourceIO PingObj )
                :<|> "dht" :> "findpeer" :> Capture "arg" TextS.Text :> StreamGet NewlineFraming JSON ( SourceIO DhtObj )
                :<|> "dht" :> "findprovs" :> Capture "arg" TextS.Text :> StreamGet NewlineFraming JSON ( SourceIO DhtObj )
                :<|> "dht" :> "get" :> Capture "arg" TextS.Text :> StreamGet NewlineFraming JSON ( SourceIO DhtObj )
                :<|> "dht" :> "provide" :> Capture "arg" TextS.Text :> StreamGet NewlineFraming JSON ( SourceIO DhtObj )
                :<|> "dht" :> "query" :>  Capture "arg" TextS.Text :>  StreamGet NewlineFraming JSON ( SourceIO DhtObj )
                :<|> "log" :> "tail" :>  StreamGet NewlineFraming IpfsText ( SourceIO LogReturnType)
                :<|> "repo" :> "gc" :>  StreamGet NewlineFraming JSON ( SourceIO RepoGcObj)
                :<|> "repo" :> "verify" :>  StreamGet NewlineFraming JSON ( SourceIO RepoVerifyObj)
                :<|> "refs" :> Capture "arg" TextS.Text :> StreamGet NewlineFraming JSON (SourceIO RefsObj)
                :<|> "refs" :> "local" :> StreamGet NewlineFraming JSON (SourceIO RefsObj)
                :<|> "pubsub" :> "sub" :>  Capture "arg" TextS.Text :>  StreamGet NewlineFraming JSON ( SourceIO PubsubSubObj )

ipfsStreamApi :: Proxy IpfsStreamApi
ipfsStreamApi =  Proxy

_ping :: TextS.Text -> ClientM (SourceIO PingObj)
_dhtFindPeer :: TextS.Text -> ClientM (SourceIO DhtObj)
_dhtFindProvs :: TextS.Text -> ClientM (SourceIO DhtObj)
_dhtGet :: TextS.Text -> ClientM (SourceIO DhtObj)
_dhtProvide :: TextS.Text -> ClientM (SourceIO DhtObj)
_dhtQuery :: TextS.Text -> ClientM (SourceIO DhtObj)
_logTail :: ClientM (SourceIO LogReturnType)
_repoGc :: ClientM (SourceIO RepoGcObj)
_repoVerify :: ClientM (SourceIO RepoVerifyObj)
_refs :: TextS.Text -> ClientM (SourceIO RefsObj)
_refsLocal :: ClientM (SourceIO RefsObj) 
_pubsubSubscribe :: TextS.Text -> ClientM (SourceIO PubsubSubObj)

_ping :<|> _dhtFindPeer :<|> _dhtFindProvs :<|> _dhtGet :<|> _dhtProvide :<|> _dhtQuery :<|>
  _logTail :<|> _repoGc :<|> _repoVerify :<|> _refs :<|> _refsLocal :<|> _pubsubSubscribe = client ipfsStreamApi