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
type ReprovideReturnType = TextS.Text
type GetReturnType = TextS.Text
type BlockReturnType = TextS.Text
type DagReturnType = TextS.Text
type ObjectReturnType = TextS.Text

data DirLink = DirLink
    { name        :: String 
    , hash        :: String
    , size        :: Int64
    , contentType :: Int
    , target      :: String
    } deriving (Show)
 
data DirObj = DirObj
    { dirHash :: String
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

data BitswapLedgerObj = BitswapLedgerObj
    {  exchanged  :: Int64
    ,  ledgerPeer :: String
    ,  recv       :: Int64
    ,  sent       :: Int64
    ,  value      :: Double
    }  deriving (Show)

data CidBasesObj = CidBasesObj
    { baseCode :: Int
    , baseName :: String
    } deriving (Show)

data CidCodecsObj = CidCodecsObj
    { codecCode :: Int
    , codecName :: String
    } deriving (Show)

data CidHashesObj = CidHashesObj
    { multihashCode :: Int
    , multihashName :: String
    } deriving (Show)

data CidObj = CidObj
    { cidStr    :: String
    , errorMsg  :: String
    , formatted :: String
    } deriving (Show)
   
data BlockStatObj = BlockStatObj
    { key       :: String
    , blockSize :: Int
    } deriving (Show)

data DagCidObj = DagCidObj {  cidSlash :: String } deriving (Show)

data DagResolveObj = DagResolveObj
    { cid     :: DagCidObj
    , remPath :: String
    } deriving (Show)

data ConfigObj = ConfigObj
    { configKey   :: String
    , configValue :: String
    } deriving (Show)

data ObjectLinkObj = ObjectLinkObj
    { linkHash  :: String
    , linkName  :: String
    , linkSize  :: Int64
    } deriving (Show)

data ObjectNewObj = ObjectNewObj { newObjectHash  :: String } deriving (Show)

data ObjectLinksObj = ObjectLinksObj
    { objectHash  :: String
    , objectLinks :: [ObjectLinkObj]   
    } deriving (Show)

data ObjectGetObj = ObjectGetObj
    { objectName     :: String
    , objectGetLinks :: [ObjectLinkObj]   
    } deriving (Show)

data ObjectStatObj = ObjectStatObj
    {  objBlockSize   :: Int
    ,  cumulativeSize :: Int
    ,  dataSize       :: Int
    ,  objHash        :: String
    ,  linksSize      :: Int
    ,  numLinks       :: Int    
    }  deriving (Show)

data PinObj = PinObj { pins  :: [String] } deriving (Show)

data BootstrapObj = BootstrapObj { bootstrapPeers  :: [String] } deriving (Show)

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

instance FromJSON BitswapLedgerObj where
    parseJSON (Object o) =
        BitswapLedgerObj  <$> o .: "Exchanged"
                          <*> o .: "Peer"
                          <*> o .: "Recv"
                          <*> o .: "Sent"
                          <*> o .: "Value"
    
    parseJSON _ = mzero

instance FromJSON CidBasesObj where
    parseJSON (Object o) =
        CidBasesObj  <$> o .: "Code"
                     <*> o .: "Name"
    
    parseJSON _ = mzero

instance FromJSON CidCodecsObj where
    parseJSON (Object o) =
        CidCodecsObj  <$> o .: "Code"
                      <*> o .: "Name"
    
    parseJSON _ = mzero

instance FromJSON CidHashesObj where
    parseJSON (Object o) =
        CidHashesObj  <$> o .: "Code"
                      <*> o .: "Name"
    
    parseJSON _ = mzero

instance FromJSON CidObj where
    parseJSON (Object o) =
        CidObj  <$> o .: "CidStr"
                <*> o .: "ErrorMsg"
                <*> o .: "Formatted"
    
    parseJSON _ = mzero

instance FromJSON BlockStatObj where
    parseJSON (Object o) =
        BlockStatObj  <$> o .: "Key"
                      <*> o .: "Size"
    
    parseJSON _ = mzero

instance FromJSON DagCidObj where
    parseJSON (Object o) =
        DagCidObj  <$> o .: "/"

    parseJSON _ = mzero
   
instance FromJSON DagResolveObj where
    parseJSON (Object o) =
        DagResolveObj  <$> o .: "Cid"
                       <*> o .: "RemPath"
    
    parseJSON _ = mzero

instance FromJSON ConfigObj where
    parseJSON (Object o) =
        ConfigObj  <$> o .: "Key"
                   <*> o .: "Value"
    
    parseJSON _ = mzero

instance FromJSON ObjectLinkObj where
    parseJSON (Object o) =
        ObjectLinkObj  <$> o .: "Hash"
                       <*> o .: "Name"
                       <*> o .: "Size"
    
    parseJSON _ = mzero

instance FromJSON ObjectNewObj where
    parseJSON (Object o) =
        ObjectNewObj  <$> o .: "Hash"
    
    parseJSON _ = mzero

instance FromJSON ObjectLinksObj where
    parseJSON (Object o) =
        ObjectLinksObj  <$> o .: "Hash"
                       <*> o .: "Links"
    
    parseJSON _ = mzero

instance FromJSON ObjectGetObj where
    parseJSON (Object o) =
        ObjectGetObj  <$> o .: "Data"
                      <*> o .: "Links"
    
    parseJSON _ = mzero

instance FromJSON ObjectStatObj where
    parseJSON (Object o) =
        ObjectStatObj  <$> o .: "BlockSize"
                       <*> o .: "CumulativeSize"
                       <*> o .: "DataSize"
                       <*> o .: "Hash"
                       <*> o .: "LinksSize"
                       <*> o .: "NumLinks"
    
    parseJSON _ = mzero

instance FromJSON PinObj where
    parseJSON (Object o) =
        PinObj  <$> o .: "Pins"
    
    parseJSON _ = mzero    

instance FromJSON BootstrapObj where
    parseJSON (Object o) =
        BootstrapObj  <$> o .: "Peers"
    
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

type IpfsApi = "cat" :> Capture "cid" TextS.Text :> Get '[IpfsText] CatReturnType
            :<|> "ls" :> Capture "cid" TextS.Text :> Get '[JSON] LsObj
            :<|> "refs" :> Capture "cid" TextS.Text :> Get '[JSON] (Vec.Vector RefsObj)
            :<|> "refs" :> "local" :> Get '[JSON] (Vec.Vector RefsObj)
            :<|> "swarm" :> "peers" :> Get '[JSON] SwarmObj
            :<|> "bitswap" :> "stat" :> Get '[JSON] BitswapStatObj
            :<|> "bitswap" :> "wantlist" :> Get '[JSON] BitswapWLObj
            :<|> "bitswap" :> "ledger" :> Capture "peerId" TextS.Text :> Get '[JSON] BitswapLedgerObj
            :<|> "bitswap" :> "reprovide" :> Get '[IpfsText] ReprovideReturnType
            :<|> "cid" :> "bases" :> Get '[JSON] [CidBasesObj]
            :<|> "cid" :> "codecs" :> Get '[JSON] [CidCodecsObj]
            :<|> "cid" :> "hashes" :> Get '[JSON] [CidHashesObj]
            :<|> "cid" :> "base32" :> Capture "cid" TextS.Text :> Get '[JSON] CidObj
            :<|> "cid" :> "format" :> Capture "cid" TextS.Text :> Get '[JSON] CidObj
            :<|> "block" :> "get" :> Capture "key" TextS.Text :> Get '[IpfsText] BlockReturnType
            :<|> "block" :> "stat" :> Capture "key" TextS.Text :> Get '[JSON] BlockStatObj
            :<|> "dag" :> "get" :> Capture "ref" TextS.Text :> Get '[JSON] DagReturnType 
            :<|> "dag" :> "resolve" :> Capture "ref" TextS.Text :> Get '[JSON] DagResolveObj 
            :<|> "config" :> Capture "ref" TextS.Text :> Get '[JSON] ConfigObj 
            :<|> "config" :> Capture "arg" TextS.Text :> QueryParam "arg" TextS.Text :> Get '[JSON] ConfigObj 
            :<|> "object" :> "data" :> Capture "ref" TextS.Text :> Get '[IpfsText] ObjectReturnType
            :<|> "object" :> "new" :> Get '[JSON] ObjectNewObj 
            :<|> "object" :> "links" :>  Capture "ref" TextS.Text :> Get '[JSON] ObjectLinksObj  
            :<|> "object" :> "patch" :> "add-link" :> Capture "arg" TextS.Text 
                :> QueryParam "arg" TextS.Text :> QueryParam "arg" TextS.Text
                :> Get '[JSON] ObjectLinksObj 
            :<|> "object" :> "get" :> Capture "arg" TextS.Text :> Get '[JSON] ObjectGetObj 
            :<|> "object" :> "stat" :> Capture "arg" TextS.Text :> Get '[JSON] ObjectStatObj 
            :<|> "pin" :> "add" :> Capture "arg" TextS.Text :> Get '[JSON] PinObj 
            :<|> "pin" :> "rm" :> Capture "arg" TextS.Text :> Get '[JSON] PinObj 
            :<|> "bootstrap" :> "add" :> QueryParam "arg" TextS.Text :> Get '[JSON] BootstrapObj 
            :<|> "bootstrap" :> "list" :> Get '[JSON] BootstrapObj 

ipfsApi :: Proxy IpfsApi
ipfsApi =  Proxy

_cat :: TextS.Text -> ClientM CatReturnType
_ls :: TextS.Text -> ClientM LsObj
_refs :: TextS.Text -> ClientM (Vec.Vector RefsObj)
_refsLocal :: ClientM (Vec.Vector RefsObj) 
_swarmPeers :: ClientM SwarmObj 
_bitswapStat :: ClientM BitswapStatObj 
_bitswapWL :: ClientM BitswapWLObj 
_bitswapLedger :: TextS.Text -> ClientM BitswapLedgerObj 
_bitswapReprovide :: ClientM ReprovideReturnType  
_cidBases :: ClientM [CidBasesObj]  
_cidCodecs :: ClientM [CidCodecsObj]  
_cidHashes :: ClientM [CidHashesObj]  
_cidBase32 :: TextS.Text -> ClientM CidObj  
_cidFormat :: TextS.Text -> ClientM CidObj
_blockGet :: TextS.Text -> ClientM BlockReturnType
_blockStat :: TextS.Text -> ClientM BlockStatObj
_dagGet :: TextS.Text -> ClientM DagReturnType
_dagResolve :: TextS.Text -> ClientM DagResolveObj
_configGet :: TextS.Text -> ClientM ConfigObj
_configSet :: TextS.Text -> Maybe TextS.Text -> ClientM ConfigObj
_objectData :: TextS.Text -> ClientM ObjectReturnType
_objectNew :: ClientM ObjectNewObj
_objectGetLinks :: TextS.Text -> ClientM ObjectLinksObj
_objectAddLink :: TextS.Text -> Maybe TextS.Text -> Maybe TextS.Text -> ClientM ObjectLinksObj
_objectGet :: TextS.Text -> ClientM ObjectGetObj
_objectStat :: TextS.Text -> ClientM ObjectStatObj
_pinAdd :: TextS.Text -> ClientM PinObj
_pinRemove :: TextS.Text -> ClientM PinObj
_bootstrapAdd ::Maybe TextS.Text -> ClientM BootstrapObj 
_bootstrapList ::ClientM BootstrapObj 

_cat :<|> _ls :<|> _refs :<|> _refsLocal :<|> _swarmPeers :<|> 
  _bitswapStat :<|> _bitswapWL :<|> _bitswapLedger :<|> _bitswapReprovide :<|> 
  _cidBases :<|> _cidCodecs :<|> _cidHashes :<|> _cidBase32 :<|> _cidFormat :<|> 
  _blockGet :<|> _blockStat :<|> _dagGet :<|> _dagResolve :<|> _configGet :<|> 
  _configSet :<|> _objectData :<|> _objectNew :<|> _objectGetLinks :<|> _objectAddLink :<|> 
  _objectGet :<|> _objectStat :<|> _pinAdd :<|> _pinRemove :<|> _bootstrapAdd :<|>
  _bootstrapList = client ipfsApi
