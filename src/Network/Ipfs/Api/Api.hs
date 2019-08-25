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

import           Control.Arrow                 (left)
import           Control.Error                 (fmapL)
import           Control.Monad
import           Data.Aeson
import           Data.Int
import           Data.ByteString.Lazy          (toStrict)
import qualified Data.ByteString.Lazy.Char8()
import qualified Data.HashMap.Strict           as H
import           Data.Map()                    
import           Data.Proxy           
import qualified Data.Text                     as TextS
import qualified Data.Text.Encoding            as TextS
import           Data.Typeable            
import qualified Data.Vector                   as Vec (fromList,Vector)
import           Network.HTTP.Client()
import qualified Network.HTTP.Media            as M ((//))
import           Servant.API
import           Servant.Client


type CatReturnType = TextS.Text
type ReprovideReturnType = TextS.Text
type GetReturnType = TextS.Text
type BlockReturnType = TextS.Text
type DagReturnType = TextS.Text
type ObjectReturnType = TextS.Text
type FilesReadType = TextS.Text

data DirLink = DirLink
    { name        :: TextS.Text 
    , hash        :: TextS.Text
    , size        :: Int64
    , contentType :: Int
    , target      :: TextS.Text
    } deriving (Show)
 
data DirObj = DirObj
    { dirHash :: TextS.Text
    , links   :: [DirLink] 
    } deriving (Show)

data LsObj = LsObj {  objs :: [DirObj]  } deriving (Show)


data RefsObj = RefsObj TextS.Text deriving (Show)
{--    {   error :: TextS.Text
    ,   ref   :: TextS.Text 
    } deriving (Show)
--}

data SwarmStreamObj = SwarmStreamObj {  protocol :: TextS.Text  } deriving (Show)  

data SwarmPeerObj = SwarmPeerObj
   {  address   :: TextS.Text
    , direction :: Int
    , latency   :: TextS.Text
    , muxer     :: TextS.Text
    , peer      :: TextS.Text
    , streams   :: Maybe [SwarmStreamObj]
   } deriving (Show)

data SwarmPeersObj = SwarmPeersObj {  peers :: [SwarmPeerObj]  } deriving (Show)  

data SwarmObj = SwarmObj {  strings :: [TextS.Text]  } deriving (Show)  

data WantlistObj = WantlistObj {  forSlash :: TextS.Text } deriving (Show)

data BitswapStatObj = BitswapStatObj
    {  blocksReceived   :: Int64
    ,  blocksSent       :: Int64
    ,  dataReceived     :: Int64
    ,  dataSent         :: Int64
    ,  dupBlksReceived  :: Int64
    ,  dupDataReceived  :: Int64
    ,  messagesReceived :: Int64
    ,  bitswapPeers     :: [TextS.Text]
    ,  provideBufLen    :: Int
    ,  wantlist         :: [WantlistObj]
    }  deriving (Show)

data BitswapWLObj = BitswapWLObj {  bitswapKeys :: [WantlistObj] } deriving (Show)

data BitswapLedgerObj = BitswapLedgerObj
    {  exchanged  :: Int64
    ,  ledgerPeer :: TextS.Text
    ,  recv       :: Int64
    ,  sent       :: Int64
    ,  value      :: Double
    }  deriving (Show)

data CidBasesObj = CidBasesObj
    { baseCode :: Int
    , baseName :: TextS.Text
    } deriving (Show)

data CidCodecsObj = CidCodecsObj
    { codecCode :: Int
    , codecName :: TextS.Text
    } deriving (Show)

data CidHashesObj = CidHashesObj
    { multihashCode :: Int
    , multihashName :: TextS.Text
    } deriving (Show)

data CidObj = CidObj
    { cidStr    :: TextS.Text
    , errorMsg  :: TextS.Text
    , formatted :: TextS.Text
    } deriving (Show)
   
data BlockObj = BlockObj
    { key       :: TextS.Text
    , blockSize :: Int
    } deriving (Show)

data DagCidObj = DagCidObj {  cidSlash :: TextS.Text } deriving (Show)

data DagResolveObj = DagResolveObj
    { cid     :: DagCidObj
    , remPath :: TextS.Text
    } deriving (Show)

data DagPutObj = DagPutObj
    { putCid     :: DagCidObj
    } deriving (Show)

data ConfigObj = ConfigObj
    { configKey   :: TextS.Text
    , configValue :: TextS.Text
    } deriving (Show)

data ObjectLinkObj = ObjectLinkObj
    { linkHash  :: TextS.Text
    , linkName  :: TextS.Text
    , linkSize  :: Int64
    } deriving (Show)

data ObjectObj = ObjectObj { newObjectHash  :: TextS.Text } deriving (Show)

data ObjectLinksObj = WithLinks
    { objectHash  :: TextS.Text
    , objectLinks :: [ObjectLinkObj]   
    } 
    | WithoutLinks { objectHash  :: TextS.Text } deriving (Show)

data ObjectGetObj = ObjectGetObj
    { objectName     :: TextS.Text
    , objectGetLinks :: [ObjectLinkObj]   
    } deriving (Show)

data ObjectStatObj = ObjectStatObj
    {  objBlockSize   :: Int
    ,  cumulativeSize :: Int
    ,  dataSize       :: Int
    ,  objHash        :: TextS.Text
    ,  linksSize      :: Int
    ,  numLinks       :: Int    
    }  deriving (Show)

data DiffObj = DiffObj {  diffSlash :: TextS.Text } deriving (Show)

data ObjectChangeObj = ObjectChangeObj
    { after    :: Maybe DiffObj 
    , before   :: DiffObj
    , path     :: TextS.Text
    , diffType :: Int
    } deriving (Show)

data ObjectDiffObj = ObjectDiffObj {  changes :: [ObjectChangeObj] } deriving (Show)

data PinObj = WithoutProgress
    { pins  :: [TextS.Text] }  
    
    | WithProgress
    {  pins     :: [TextS.Text]
    ,  progress :: Int
    } deriving (Show)

data BootstrapObj = BootstrapObj { bootstrapPeers  :: [TextS.Text] } deriving (Show)

data StatsBwObj = StatsBwObj
    {  rateIn   :: Double
    ,  rateOut  :: Double
    ,  totalIn  :: Int64
    ,  totalOut :: Int64
    }  deriving (Show)

data StatsRepoObj = StatsRepoObj
    {  numObjects  :: Int64
    ,  repoPath    :: TextS.Text
    ,  repoSize    :: Int64
    ,  storageMax  :: Int64
    ,  repoVersion :: TextS.Text
    }  deriving (Show)

data VersionObj = VersionObj
    {  commit  :: TextS.Text
    ,  golang  :: TextS.Text
    ,  repo    :: TextS.Text
    ,  system  :: TextS.Text
    ,  version :: TextS.Text
    }  deriving (Show)

data IdObj = IdObj
    {  addresses       :: [TextS.Text]
    ,  agentVersion    :: TextS.Text
    ,  id              :: TextS.Text
    ,  protocolVersion :: TextS.Text
    ,  publicKey       :: TextS.Text
    }  deriving (Show)

data DnsObj = DnsObj { dnsPath  :: TextS.Text } deriving (Show)

data PubsubObj = PubsubObj { pubsubStrings :: [TextS.Text] } deriving (Show)  

data LogLsObj = LogLsObj { logLsStrings :: [TextS.Text] } deriving (Show)  

data LogLevelObj = LogLevelObj { message :: TextS.Text } deriving (Show)  

data RepoVersionObj = RepoVersionObj { repoVer :: TextS.Text } deriving (Show)  

data RepoFsckObj = RepoFsckObj { repoMessage :: TextS.Text } deriving (Show)  

data KeyDetailsObj = KeyDetailsObj
    { keyId    :: TextS.Text 
    , keyName  :: TextS.Text 
    } deriving (Show)

data KeyObj = KeyObj { keys :: [KeyDetailsObj] } deriving (Show)  

data KeyRenameObj = KeyRenameObj
    {  peerId     :: TextS.Text
    ,  now        :: TextS.Text
    ,  overwrite  :: Bool
    ,  was        :: TextS.Text
    }  deriving (Show)

data FilesStatObj = FilesStatObj
    {  fileObjectHash        :: TextS.Text
    ,  objectSize            :: Int
    ,  cumulativeObjectSize  :: Int
    ,  blocks                :: Int
    ,  objectType            :: TextS.Text
    }  deriving (Show)

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

instance FromJSON SwarmPeersObj where
    parseJSON (Object o) =
        SwarmPeersObj  <$> o .: "Peers"

    parseJSON _ = mzero


instance FromJSON SwarmObj where
    parseJSON (Object o) =
        SwarmObj  <$> o .: "Strings"
    
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

instance FromJSON BlockObj where
    parseJSON (Object o) =
        BlockObj  <$> o .: "Key"
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

instance FromJSON DagPutObj where
    parseJSON (Object o) =
        DagPutObj  <$> o .: "Cid"
    
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

instance FromJSON ObjectObj where
    parseJSON (Object o) =
        ObjectObj  <$> o .: "Hash"
    
    parseJSON _ = mzero

instance FromJSON ObjectLinksObj where
    parseJSON (Object v) =
        case H.lookup "Links" v of
            Just (_) -> WithLinks <$> v .: "Hash" 
                                  <*> v .: "Links"

            Nothing -> 
                case H.lookup "Hash" v of
                      Just (_) -> WithoutLinks <$> v .: "Hash" 
                      Nothing -> mzero
    
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

instance FromJSON ObjectChangeObj where
    parseJSON (Object o) =
        ObjectChangeObj  <$> o .: "After"
                         <*> o .: "Before"
                         <*> o .: "Path"
                         <*> o .: "Type"

    parseJSON _ = mzero

instance FromJSON DiffObj where
    parseJSON (Object o) =
        DiffObj  <$> o .: "/"

    parseJSON _ = mzero

instance FromJSON ObjectDiffObj where
    parseJSON (Object o) =
        ObjectDiffObj  <$> o .: "Changes"

    parseJSON _ = mzero

instance FromJSON PinObj where
    parseJSON (Object v) =
        case H.lookup "Progress" v of
            Just (_) -> WithProgress <$> v .: "Pins" 
                                     <*> v .: "Progress"

            Nothing -> 
                case H.lookup "Pins" v of
                      Just (_) -> WithoutProgress <$> v .: "Pins" 
                      Nothing -> mzero
    
    parseJSON _ = mzero

instance FromJSON BootstrapObj where
    parseJSON (Object o) =
        BootstrapObj  <$> o .: "Peers"
    
    parseJSON _ = mzero

instance FromJSON StatsBwObj where
    parseJSON (Object o) =
        StatsBwObj  <$> o .: "RateIn"
                    <*> o .: "RateOut"
                    <*> o .: "TotalIn"
                    <*> o .: "TotalOut"
    
    parseJSON _ = mzero

instance FromJSON StatsRepoObj where
    parseJSON (Object o) =
        StatsRepoObj  <$> o .: "NumObjects"
                      <*> o .: "RepoPath"
                      <*> o .: "RepoSize"
                      <*> o .: "StorageMax"
                      <*> o .: "Version"

    parseJSON _ = mzero

instance FromJSON VersionObj where
    parseJSON (Object o) =
        VersionObj  <$> o .: "Commit"
                    <*> o .: "Golang"
                    <*> o .: "Repo"
                    <*> o .: "System"
                    <*> o .: "Version"

    parseJSON _ = mzero

instance FromJSON IdObj where
    parseJSON (Object o) =
        IdObj  <$> o .: "Addresses"
               <*> o .: "AgentVersion"
               <*> o .: "ID"
               <*> o .: "ProtocolVersion"
               <*> o .: "PublicKey"

    parseJSON _ = mzero

instance FromJSON DnsObj where
    parseJSON (Object o) =
        DnsObj  <$> o .: "Path"
    
    parseJSON _ = mzero

instance FromJSON PubsubObj where
    parseJSON (Object o) =
        PubsubObj  <$> o .: "Strings"
    
    parseJSON _ = mzero

instance FromJSON LogLsObj where
    parseJSON (Object o) =
        LogLsObj  <$> o .: "Strings"
    
    parseJSON _ = mzero

instance FromJSON LogLevelObj where
    parseJSON (Object o) =
        LogLevelObj  <$> o .: "Message"
    
    parseJSON _ = mzero

instance FromJSON RepoVersionObj where
    parseJSON (Object o) =
        RepoVersionObj  <$> o .: "Version"
    
    parseJSON _ = mzero

instance FromJSON RepoFsckObj where
    parseJSON (Object o) =
        RepoFsckObj  <$> o .: "Message"
    
    parseJSON _ = mzero


instance FromJSON KeyDetailsObj where
    parseJSON (Object o) =
        KeyDetailsObj <$> o .: "Id"
                      <*> o .: "Name"
    
    parseJSON _ = mzero

instance FromJSON KeyObj where
    parseJSON (Object o) =
        KeyObj  <$> o .: "Keys"
    
    parseJSON _ = mzero

instance FromJSON KeyRenameObj where
    parseJSON (Object o) =
        KeyRenameObj  <$> o .: "Id"
                      <*> o .: "Now"
                      <*> o .: "Overwrite"
                      <*> o .: "Was"
    
    parseJSON _ = mzero

instance FromJSON FilesStatObj where
    parseJSON (Object o) =
        FilesStatObj  <$> o .: "Hash"
                      <*> o .: "Size"
                      <*> o .: "CumulativeSize"
                      <*> o .: "Blocks"
                      <*> o .: "Type"
    
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


-- | Defining a content type same as IpfsJSON 
data IpfsJSON deriving Typeable

instance Servant.API.Accept IpfsJSON where
    contentType _ = "application" M.// "json"

-- | @left show . TextS.decodeUtf8' . toStrict@
instance MimeUnrender IpfsJSON TextS.Text where
    mimeUnrender _ = left show . TextS.decodeUtf8' . toStrict

instance {-# OVERLAPPING #-} MimeUnrender JSON (Vec.Vector RefsObj) where
  mimeUnrender _ bs = do
    t <- fmapL show (TextS.decodeUtf8' (toStrict bs))
    pure (Vec.fromList (map RefsObj (map TextS.pack (lines $ TextS.unpack t))))    

type IpfsApi = "cat" :> Capture "arg" TextS.Text :> Get '[IpfsText] CatReturnType
            :<|> "ls" :> Capture "arg" TextS.Text :> Get '[JSON] LsObj
            :<|> "get" :> Capture "arg" TextS.Text :> Get '[IpfsText] GetReturnType
            :<|> "refs" :> Capture "arg" TextS.Text :> Get '[JSON] (Vec.Vector RefsObj)
            :<|> "refs" :> "local" :> Get '[JSON] (Vec.Vector RefsObj)
            :<|> "swarm" :> "peers" :> Get '[JSON] SwarmPeersObj
            :<|> "swarm" :> "connect" :> QueryParam "arg" TextS.Text :> Get '[JSON] SwarmObj 
            :<|> "swarm" :> "disconnect" :> QueryParam "arg" TextS.Text :> Get '[JSON] SwarmObj 
            :<|> "swarm" :> "filters" :> Get '[JSON] SwarmObj
            :<|> "swarm" :> "filters" :> "add" :> QueryParam "arg" TextS.Text :> Get '[JSON] SwarmObj 
            :<|> "swarm" :> "filters" :> "rm" :> QueryParam "arg" TextS.Text :> Get '[JSON] SwarmObj 
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
            :<|> "block" :> "stat" :> Capture "key" TextS.Text :> Get '[JSON] BlockObj
            :<|> "dag" :> "get" :> Capture "ref" TextS.Text :> Get '[IpfsJSON] DagReturnType 
            :<|> "dag" :> "resolve" :> Capture "ref" TextS.Text :> Get '[JSON] DagResolveObj 
            :<|> "config" :> Capture "ref" TextS.Text :> Get '[JSON] ConfigObj 
            :<|> "config" :> Capture "arg" TextS.Text :> QueryParam "arg" TextS.Text :> Get '[JSON] ConfigObj 
            :<|> "object" :> "data" :> Capture "ref" TextS.Text :> Get '[IpfsText] ObjectReturnType
            :<|> "object" :> "new" :> Get '[JSON] ObjectObj 
            :<|> "object" :> "links" :>  Capture "ref" TextS.Text :> Get '[JSON] ObjectLinksObj  
            :<|> "object" :> "patch" :> "add-link" :> Capture "arg" TextS.Text 
                :> QueryParam "arg" TextS.Text :> QueryParam "arg" TextS.Text :> Get '[JSON] ObjectLinksObj 
            :<|> "object" :> "patch" :> "rm-link" :> Capture "arg" TextS.Text 
                :> QueryParam "arg" TextS.Text :> Get '[JSON] ObjectLinksObj 
            :<|> "object" :> "get" :> Capture "arg" TextS.Text :> Get '[JSON] ObjectGetObj 
            :<|> "object" :> "diff" :> Capture "arg" TextS.Text :> QueryParam "arg" TextS.Text :> Get '[JSON] ObjectDiffObj 
            :<|> "object" :> "stat" :> Capture "arg" TextS.Text :> Get '[JSON] ObjectStatObj 
            :<|> "pin" :> "add" :> Capture "arg" TextS.Text :> Get '[JSON] PinObj 
            :<|> "pin" :> "rm" :> Capture "arg" TextS.Text :> Get '[JSON] PinObj 
            :<|> "bootstrap" :> "add" :> QueryParam "arg" TextS.Text :> Get '[JSON] BootstrapObj 
            :<|> "bootstrap" :> "list" :> Get '[JSON] BootstrapObj 
            :<|> "bootstrap" :> "rm" :> QueryParam "arg" TextS.Text :> Get '[JSON] BootstrapObj 
            :<|> "stats" :> "bw" :> Get '[JSON] StatsBwObj 
            :<|> "stats" :> "repo" :> Get '[JSON] StatsRepoObj 
            :<|> "version" :> Get '[JSON] VersionObj 
            :<|> "id" :> Get '[JSON] IdObj 
            :<|> "id" :> Capture "arg" TextS.Text :> Get '[JSON] IdObj 
            :<|> "dns" :> Capture "arg" TextS.Text :> Get '[JSON] DnsObj 
            :<|> "pubsub" :> "ls" :>  Get '[JSON] PubsubObj 
            :<|> "pubsub" :> "peers" :>  Get '[JSON] PubsubObj 
            :<|> "log" :> "ls" :>  Get '[JSON] LogLsObj 
            :<|> "log" :> "level" :> Capture "arg" TextS.Text :> QueryParam "arg" TextS.Text :> Get '[JSON] LogLevelObj 
            :<|> "repo" :> "version" :>  Get '[JSON] RepoVersionObj 
            :<|> "repo" :> "fsck" :>  Get '[JSON] RepoFsckObj 
            :<|> "key" :> "gen" :> Capture "arg" TextS.Text :> QueryParam "type" TextS.Text :> Get '[JSON] KeyDetailsObj 
            :<|> "key" :> "list" :>  Get '[JSON] KeyObj 
            :<|> "key" :> "rename" :> Capture "arg" TextS.Text :> QueryParam "arg" TextS.Text :> Get '[JSON] KeyRenameObj 
            :<|> "key" :> "rm" :> Capture "arg" TextS.Text :> Get '[JSON] KeyObj 
            :<|> "files" :> "cp" :> QueryParam "arg" TextS.Text :> QueryParam "arg" TextS.Text :> Get '[IpfsJSON] NoContent 
            :<|> "files" :> "mkdir" :> QueryParam "arg" TextS.Text :> Get '[IpfsJSON] NoContent 
            :<|> "files" :> "read" :> QueryParam "arg" TextS.Text :> Get '[IpfsText] FilesReadType 
            :<|> "files" :> "rm" :> QueryParam "arg" TextS.Text :> QueryParam "recursive" Bool :> Get '[JSON] NoContent 
            :<|> "files" :> "stat" :> QueryParam "arg" TextS.Text :> Get '[JSON] FilesStatObj 
            :<|> "shutdown" :> Get '[JSON] NoContent 

ipfsApi :: Proxy IpfsApi
ipfsApi =  Proxy

_cat :: TextS.Text -> ClientM CatReturnType
_ls :: TextS.Text -> ClientM LsObj
_get :: TextS.Text -> ClientM GetReturnType
_refs :: TextS.Text -> ClientM (Vec.Vector RefsObj)
_refsLocal :: ClientM (Vec.Vector RefsObj) 
_swarmPeers :: ClientM SwarmPeersObj 
_swarmConnect :: Maybe TextS.Text -> ClientM SwarmObj 
_swarmDisconnect :: Maybe TextS.Text -> ClientM SwarmObj 
_swarmFilters :: ClientM SwarmObj 
_swarmFilterAdd :: Maybe TextS.Text -> ClientM SwarmObj 
_swarmFilterRm :: Maybe TextS.Text -> ClientM SwarmObj 
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
_blockStat :: TextS.Text -> ClientM BlockObj
_dagGet :: TextS.Text -> ClientM DagReturnType
_dagResolve :: TextS.Text -> ClientM DagResolveObj
_configGet :: TextS.Text -> ClientM ConfigObj
_configSet :: TextS.Text -> Maybe TextS.Text -> ClientM ConfigObj
_objectData :: TextS.Text -> ClientM ObjectReturnType
_objectNew :: ClientM ObjectObj
_objectGetLinks :: TextS.Text -> ClientM ObjectLinksObj
_objectAddLink :: TextS.Text -> Maybe TextS.Text -> Maybe TextS.Text -> ClientM ObjectLinksObj
_objectRmLink :: TextS.Text -> Maybe TextS.Text -> ClientM ObjectLinksObj
_objectGet :: TextS.Text -> ClientM ObjectGetObj
_objectDiff :: TextS.Text -> Maybe TextS.Text -> ClientM ObjectDiffObj
_objectStat :: TextS.Text -> ClientM ObjectStatObj
_pinAdd :: TextS.Text -> ClientM PinObj
_pinRemove :: TextS.Text -> ClientM PinObj
_bootstrapAdd ::Maybe TextS.Text -> ClientM BootstrapObj 
_bootstrapList :: ClientM BootstrapObj 
_bootstrapRM :: Maybe TextS.Text -> ClientM BootstrapObj 
_statsBw :: ClientM StatsBwObj 
_statsRepo :: ClientM StatsRepoObj 
_version :: ClientM VersionObj 
_id :: ClientM IdObj 
_idPeer :: TextS.Text -> ClientM IdObj 
_dns :: TextS.Text -> ClientM DnsObj 
_pubsubLs :: ClientM PubsubObj 
_pubsubPeers :: ClientM PubsubObj 
_logLs :: ClientM LogLsObj 
_logLevel :: TextS.Text -> Maybe TextS.Text -> ClientM LogLevelObj
_repoVersion :: ClientM RepoVersionObj 
_repoFsck :: ClientM RepoFsckObj 
_keyGen :: TextS.Text -> (Maybe TextS.Text) -> ClientM KeyDetailsObj 
_keyList :: ClientM KeyObj 
_keyRename :: TextS.Text -> (Maybe TextS.Text) -> ClientM KeyRenameObj 
_keyRm :: TextS.Text -> ClientM KeyObj 
_filesCp :: Maybe TextS.Text -> Maybe TextS.Text -> ClientM NoContent 
_filesMkdir :: Maybe TextS.Text -> ClientM NoContent 
_filesRead :: Maybe TextS.Text -> ClientM FilesReadType 
_filesRm :: Maybe TextS.Text -> Maybe Bool -> ClientM NoContent 
_filesStat :: Maybe TextS.Text -> ClientM FilesStatObj 
_shutdown :: ClientM NoContent 

_cat :<|> _ls :<|> _get :<|> _refs :<|> _refsLocal :<|> _swarmPeers :<|> _swarmConnect :<|> _swarmDisconnect :<|>
  _swarmFilters :<|> _swarmFilterAdd :<|> _swarmFilterRm :<|>  _bitswapStat :<|> _bitswapWL :<|> _bitswapLedger :<|> 
  _bitswapReprovide :<|> _cidBases :<|> _cidCodecs :<|> _cidHashes :<|> _cidBase32 :<|> _cidFormat :<|> 
  _blockGet  :<|> _blockStat :<|> _dagGet :<|> _dagResolve :<|> _configGet :<|> 
  _configSet :<|> _objectData :<|> _objectNew :<|> _objectGetLinks :<|> _objectAddLink :<|> _objectRmLink :<|> 
  _objectGet :<|> _objectDiff :<|> _objectStat :<|> _pinAdd :<|> _pinRemove :<|> _bootstrapAdd :<|>
  _bootstrapList :<|> _bootstrapRM :<|> _statsBw :<|> _statsRepo :<|> _version :<|> _id :<|> _idPeer :<|>
  _dns :<|> _pubsubLs :<|> _pubsubPeers :<|> _logLs :<|> _logLevel :<|> _repoVersion :<|> 
  _repoFsck :<|> _keyGen :<|> _keyList :<|> _keyRename :<|> _keyRm :<|> _filesCp :<|> _filesMkdir :<|> 
  _filesRead :<|> _filesRm :<|> _filesStat :<|> _shutdown = client ipfsApi
