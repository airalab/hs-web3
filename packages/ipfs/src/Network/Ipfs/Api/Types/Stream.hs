{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

-- |
-- Module      :  Network.Ipfs.Api.Types.Stream
-- Copyright   :  Aleksandr Krupenkin 2016-2021
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Ipfs Stream API types.
--

module Network.Ipfs.Api.Types.Stream where

import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 ()
import           Data.Int
import           Data.Text
import           Network.HTTP.Client        ()
import           Servant.API

import           Network.Ipfs.Api.Types     (IpfsText)

type LogReturnType = Text

data PingObj = PingObj
    { success :: Bool
    , text    :: Text
    , time    :: Int64
    }
    deriving (Show, Eq)

data ResponseObj = ResponseObj
    { addrs :: Maybe [Text]
    , id    :: Text
    }
    deriving (Show, Eq)

data DhtObj = DhtObj
    { extra     :: Text
    , addrid    :: Text
    , responses :: Maybe [ResponseObj]
    , addrType  :: Int
    }
    deriving (Show, Eq)

data RepoKeyObj = RepoKeyObj
    { repoSlash :: Text
    }
    deriving (Show, Eq)

data RepoGcObj = RepoGcObj
    { repoKey :: RepoKeyObj
    }
    deriving (Show, Eq)

data RepoVerifyObj = RepoVerifyObj
    { msg      :: Text
    , progress :: Int
    }
    deriving (Show, Eq)

data RefsObj = RefsObj
    { error :: Text
    , ref   :: Text
    }
    deriving (Show, Eq)

data PubsubSubObj = PubsubSubObj
    { mssgdata :: Text
    , from     :: Text
    , seqno    :: Text
    , topicIDs :: [Text]
    }
    deriving (Show, Eq)

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

type IpfsStreamApi = "ping" :> Capture "arg" Text :> StreamGet NewlineFraming JSON ( SourceIO PingObj )
                :<|> "dht" :> "findpeer" :> Capture "arg" Text :> StreamGet NewlineFraming JSON ( SourceIO DhtObj )
                :<|> "dht" :> "findprovs" :> Capture "arg" Text :> StreamGet NewlineFraming JSON ( SourceIO DhtObj )
                :<|> "dht" :> "get" :> Capture "arg" Text :> StreamGet NewlineFraming JSON ( SourceIO DhtObj )
                :<|> "dht" :> "provide" :> Capture "arg" Text :> StreamGet NewlineFraming JSON ( SourceIO DhtObj )
                :<|> "dht" :> "query" :>  Capture "arg" Text :>  StreamGet NewlineFraming JSON ( SourceIO DhtObj )
                :<|> "log" :> "tail" :>  StreamGet NewlineFraming IpfsText ( SourceIO LogReturnType)
                :<|> "repo" :> "gc" :>  StreamGet NewlineFraming JSON ( SourceIO RepoGcObj)
                :<|> "repo" :> "verify" :>  StreamGet NewlineFraming JSON ( SourceIO RepoVerifyObj)
                :<|> "refs" :> Capture "arg" Text :> StreamGet NewlineFraming JSON (SourceIO RefsObj)
                :<|> "refs" :> "local" :> StreamGet NewlineFraming JSON (SourceIO RefsObj)
                :<|> "pubsub" :> "sub" :>  Capture "arg" Text :>  StreamGet NewlineFraming JSON ( SourceIO PubsubSubObj )
