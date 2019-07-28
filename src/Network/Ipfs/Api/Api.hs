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
import           Control.Error                    (fmapL)
import           Control.Monad
import           Data.Aeson
import           Data.Int
import           Data.ByteString.Lazy             (toStrict)
import qualified Data.ByteString.Lazy.Char8()
import           Data.Proxy           
import qualified Data.Text                        as TextS
import qualified Data.Text.Encoding               as TextS
import           Data.Typeable            
import qualified Data.Vector                      as Vec (fromList,Vector)
import           Network.HTTP.Client()
import qualified Network.HTTP.Media               as M ((//))
import           Servant.API
import           Servant.Client


type CatReturnType = TextS.Text


data DirLink = DirLink
    { name        :: String 
    , hash        :: String
    , size        :: Int64
    , contentType :: Int
    , target      :: String
    } deriving (Show)
 
data DirObj = DirObj
    { objHash :: String
    , links   :: [DirLink] 
    } deriving (Show)

data LsObj = LsObj {  objs :: [DirObj]  } deriving (Show)


data RefsObj = RefsObj String deriving (Show)
{--    {   error :: String
    ,   ref   :: String 
    } deriving (Show)
--}

data SwarmStreamObj = SwarmStreamObj {  protocol :: String  } deriving (Show)  

data SwarmPeerObj = SwarmPeerObj
   {  address   :: String
    , direction :: Int
    , latency   :: String
    , muxer     :: String
    , peer      :: String
    , streams   :: Maybe [SwarmStreamObj]
   } deriving (Show)

data SwarmObj = SwarmObj {  peers :: [SwarmPeerObj]  } deriving (Show)  


data WantlistObj = WantlistObj {  forSlash :: String } deriving (Show)

data BitswapStatObj = BitswapStatObj
    {  blocksReceived   :: Int64
    ,  blocksSent       :: Int64
    ,  dataReceived     :: Int64
    ,  dataSent         :: Int64
    ,  dupBlksReceived  :: Int64
    ,  dupDataReceived  :: Int64
    ,  messagesReceived :: Int64
    ,  bitswapPeers     :: [String]
    ,  provideBufLen    :: Int
    ,  wantlist         :: [WantlistObj]
    }  deriving (Show)

data BitswapWLObj = BitswapWLObj {  keys :: [WantlistObj] } deriving (Show)

instance FromJSON DirLink where
    parseJSON (Object o) =
        DirLink  <$> o .: "Name"
                 <*> o .: "Hash"
                 <*> o .: "Size"
                 <*> o .: "Type"
                 <*> o .: "Target"
    
    parseJSON _ = mzero

instance FromJSON DirObj where
    parseJSON (Object o) =
        DirObj  <$> o .: "Hash"
                <*> o .: "Links"
    
    parseJSON _ = mzero

instance FromJSON LsObj where
    parseJSON (Object o) =
        LsObj  <$> o .: "Objects"

    parseJSON _ = mzero


instance FromJSON SwarmStreamObj where
    parseJSON (Object o) =
        SwarmStreamObj  <$> o .: "Protocol"

    parseJSON _ = mzero    

instance FromJSON SwarmPeerObj where
    parseJSON (Object o) =
        SwarmPeerObj  <$> o .: "Addr"
                      <*> o .: "Direction"
                      <*> o .: "Latency"
                      <*> o .: "Muxer"
                      <*> o .: "Peer"
                      <*> o .: "Streams"
    
    parseJSON _ = mzero

instance FromJSON SwarmObj where
    parseJSON (Object o) =
        SwarmObj  <$> o .: "Peers"

    parseJSON _ = mzero


instance FromJSON WantlistObj where
    parseJSON (Object o) =
        WantlistObj  <$> o .: "/"

    parseJSON _ = mzero

instance FromJSON BitswapStatObj where
    parseJSON (Object o) =
        BitswapStatObj  <$> o .: "BlocksReceived"
                        <*> o .: "BlocksSent"
                        <*> o .: "DataReceived"
                        <*> o .: "DataSent"
                        <*> o .: "DupBlksReceived"
                        <*> o .: "DupDataReceived"
                        <*> o .: "MessagesReceived"
                        <*> o .: "Peers"
                        <*> o .: "ProvideBufLen"
                        <*> o .: "Wantlist"
    
    parseJSON _ = mzero

    
instance FromJSON BitswapWLObj where
    parseJSON (Object o) =
        BitswapWLObj  <$> o .: "Keys"

    parseJSON _ = mzero
    
{--
instance FromJSON RefsObj where
    parseJSON (Objecto o) =
        RefsObj  <$> o .: "Err"
                    <*> o .: "Ref"

    parseJSON _ = mzero
--}

-- | Defining a content type same as PlainText without charset
data IpfsText deriving Typeable

instance Servant.API.Accept IpfsText where
    contentType _ = "text" M.// "plain"

-- | @left show . TextS.decodeUtf8' . toStrict@
instance MimeUnrender IpfsText TextS.Text where
    mimeUnrender _ = left show . TextS.decodeUtf8' . toStrict

instance {-# OVERLAPPING #-} MimeUnrender JSON (Vec.Vector RefsObj) where
  mimeUnrender _ bs = do
    t <- fmapL show (TextS.decodeUtf8' (toStrict bs))
    pure (Vec.fromList (map RefsObj (lines $ TextS.unpack t)))    

type IpfsApi = "cat" :> Capture "cid" String :> Get '[IpfsText] CatReturnType
            :<|> "ls" :> Capture "cid" String :> Get '[JSON] LsObj
            :<|> "refs" :> Capture "cid" String :> Get '[JSON] (Vec.Vector RefsObj)
            :<|> "refs" :> "local" :> Get '[JSON] (Vec.Vector RefsObj)
            :<|> "swarm" :> "peers" :> Get '[JSON] SwarmObj
            :<|> "bitswap" :> "stat" :> Get '[JSON] BitswapStatObj
            :<|> "bitswap" :> "wantlist" :> Get '[JSON] BitswapWLObj

ipfsApi :: Proxy IpfsApi
ipfsApi =  Proxy

_cat :: String -> ClientM CatReturnType
_ls :: String -> ClientM LsObj
_refs :: String -> ClientM (Vec.Vector RefsObj)
_refsLocal :: ClientM (Vec.Vector RefsObj) 
_swarmPeers :: ClientM SwarmObj 
_bitswapStat :: ClientM BitswapStatObj 
_bitswapWL :: ClientM BitswapWLObj 

_cat :<|> _ls :<|> _refs :<|> _refsLocal :<|> _swarmPeers :<|> 
  _bitswapStat :<|> _bitswapWL  = client ipfsApi
