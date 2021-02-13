{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

-- |
-- Module      :  Network.Ipfs.Api.Types
-- Copyright   :  Aleksandr Krupenkin 2016-2021
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- IPFS API types.
--

module Network.Ipfs.Api.Types where

import           Control.Arrow              (left)
import           Control.Monad
import           Data.Aeson
import           Data.ByteString.Lazy       (toStrict)
import qualified Data.ByteString.Lazy.Char8 ()
import qualified Data.HashMap.Strict        as H
import           Data.Int
import           Data.Text                  (Text)
import qualified Data.Text.Encoding         as TextS
import           Data.Typeable
import           Network.HTTP.Client        ()
import qualified Network.HTTP.Media         as M ((//))
import           Servant.API

type CatReturnType = Text
type ReprovideReturnType = Text
type GetReturnType = Text
type BlockReturnType = Text
type DagReturnType = Text
type ObjectReturnType = Text
type FilesReadType = Text

data DirLink = DirLink
    { dlName        :: Text
    , dlHash        :: Text
    , dlSize        :: Int64
    , dlContentType :: Int
    , dlTarget      :: Text
    }
    deriving (Show, Eq)

data DirObj = DirObj
    { dirHash :: Text
    , links   :: [DirLink]
    }
    deriving (Show, Eq)

data AddObj = AddObj
    { name :: Text
    , hash :: Text
    , size :: Text
    }
    deriving (Show, Eq)

instance FromJSON AddObj where
    parseJSON (Object o) =
        AddObj  <$> o .: "Name"
                <*> o .: "Hash"
                <*> o .: "Size"

    parseJSON _ = mzero

data LsObj = LsObj
    { objs :: [DirObj]
    }
    deriving (Show, Eq)

data SwarmStreamObj = SwarmStreamObj
    { protocol :: Text
    }
    deriving (Show, Eq)

data SwarmPeerObj = SwarmPeerObj
    { address   :: Text
    , direction :: Int
    , latency   :: Text
    , muxer     :: Text
    , peer      :: Text
    , streams   :: Maybe [SwarmStreamObj]
    }
    deriving (Show, Eq)

data SwarmPeersObj = SwarmPeersObj
    { peers :: [SwarmPeerObj]
    }
    deriving (Show, Eq)

data SwarmObj = SwarmObj
    { strings :: [Text]
    }
    deriving (Show, Eq)

data WantlistObj = WantlistObj
    { forSlash :: Text
    }
    deriving (Show, Eq)

data BitswapStatObj = BitswapStatObj
    { blocksReceived   :: Int64
    , blocksSent       :: Int64
    , dataReceived     :: Int64
    , dataSent         :: Int64
    , dupBlksReceived  :: Int64
    , dupDataReceived  :: Int64
    , messagesReceived :: Int64
    , bitswapPeers     :: [Text]
    , provideBufLen    :: Int
    , wantlist         :: [WantlistObj]
    }
    deriving (Show, Eq)

data BitswapWLObj = BitswapWLObj
    { bitswapKeys :: [WantlistObj]
    }
    deriving (Show, Eq)

data BitswapLedgerObj = BitswapLedgerObj
    { exchanged  :: Int64
    , ledgerPeer :: Text
    , recv       :: Int64
    , sent       :: Int64
    , value      :: Double
    }
    deriving (Show, Eq)

data CidBasesObj = CidBasesObj
    { baseCode :: Int
    , baseName :: Text
    }
    deriving (Show, Eq)

data CidCodecsObj = CidCodecsObj
    { codecCode :: Int
    , codecName :: Text
    }
    deriving (Show, Eq)

data CidHashesObj = CidHashesObj
    { multihashCode :: Int
    , multihashName :: Text
    }
    deriving (Show, Eq)

data CidObj = CidObj
    { cidStr    :: Text
    , errorMsg  :: Text
    , formatted :: Text
    }
    deriving (Show, Eq)

data BlockObj = BlockObj
    { key       :: Text
    , blockSize :: Int
    }
    deriving (Show, Eq)

data DagCidObj = DagCidObj
    { cidSlash :: Text
    }
    deriving (Show, Eq)

data DagResolveObj = DagResolveObj
    { cid     :: DagCidObj
    , remPath :: Text
    }
    deriving (Show, Eq)

data DagPutObj = DagPutObj
    { putCid :: DagCidObj
    }
    deriving (Show, Eq)

data ConfigObj = ConfigObj
    { configKey   :: Text
    , configValue :: Text
    }
    deriving (Show, Eq)

data ObjectLinkObj = ObjectLinkObj
    { linkHash :: Text
    , linkName :: Text
    , linkSize :: Int64
    }
    deriving (Show, Eq)

data ObjectObj = ObjectObj
    { newObjectHash :: Text
    }
    deriving (Show, Eq)

data ObjectLinksObj = WithLinks
    { objectHash  :: Text
    , objectLinks :: [ObjectLinkObj]
    }
    | WithoutLinks
    { objectHash :: Text
    }
    deriving (Show, Eq)

data ObjectGetObj = ObjectGetObj
    { objectName     :: Text
    , objectGetLinks :: [ObjectLinkObj]
    }
    deriving (Show, Eq)

data ObjectStatObj = ObjectStatObj
    { objBlockSize   :: Int
    , cumulativeSize :: Int
    , dataSize       :: Int
    , objHash        :: Text
    , linksSize      :: Int
    , numLinks       :: Int
    }
    deriving (Show, Eq)

data DiffObj = DiffObj
    { diffSlash :: Text
    }
    deriving (Show, Eq)

data ObjectChangeObj = ObjectChangeObj
    { after    :: Maybe DiffObj
    , before   :: DiffObj
    , path     :: Text
    , diffType :: Int
    }
    deriving (Show, Eq)

data ObjectDiffObj = ObjectDiffObj
    { changes :: [ObjectChangeObj]
    }
    deriving (Show, Eq)

data PinObj = WithoutProgress
    { pins :: [Text]
    }
    | WithProgress
    { pins     :: [Text]
    , progress :: Int
    }
    deriving (Show, Eq)

data BootstrapObj = BootstrapObj
    { bootstrapPeers :: [Text]
    }
    deriving (Show, Eq)

data StatsBwObj = StatsBwObj
    { rateIn   :: Double
    , rateOut  :: Double
    , totalIn  :: Int64
    , totalOut :: Int64
    }
    deriving (Show, Eq)

data StatsRepoObj = StatsRepoObj
    { numObjects  :: Int64
    , repoPath    :: Text
    , repoSize    :: Int64
    , storageMax  :: Int64
    , repoVersion :: Text
    }
    deriving (Show, Eq)

data VersionObj = VersionObj
    { commit  :: Text
    , golang  :: Text
    , repo    :: Text
    , system  :: Text
    , version :: Text
    }
    deriving (Show, Eq)

data IdObj = IdObj
    { addresses       :: [Text]
    , agentVersion    :: Text
    , id              :: Text
    , protocolVersion :: Text
    , publicKey       :: Text
    }
    deriving (Show, Eq)

data DnsObj = DnsObj
    { dnsPath :: Text
    }
    deriving (Show, Eq)

data PubsubObj = PubsubObj
    { pubsubStrings :: [Text]
    }
    deriving (Show, Eq)

data LogLsObj = LogLsObj
    { logLsStrings :: [Text]
    }
    deriving (Show, Eq)

data LogLevelObj = LogLevelObj
    { message :: Text
    }
    deriving (Show, Eq)

data RepoVersionObj = RepoVersionObj
    { repoVer :: Text
    }
    deriving (Show, Eq)

data RepoFsckObj = RepoFsckObj
    { repoMessage :: Text
    }
    deriving (Show, Eq)

data KeyDetailsObj = KeyDetailsObj
    { keyId   :: Text
    , keyName :: Text
    }
    deriving (Show, Eq)

data KeyObj = KeyObj
    { keys :: [KeyDetailsObj]
    }
    deriving (Show, Eq)

data KeyRenameObj = KeyRenameObj
    { peerId    :: Text
    , now       :: Text
    , overwrite :: Bool
    , was       :: Text
    }
    deriving (Show, Eq)

data FilesStatObj = FilesStatObj
    { fileObjectHash       :: Text
    , objectSize           :: Int
    , cumulativeObjectSize :: Int
    , blocks               :: Int
    , objectType           :: Text
    }
    deriving (Show, Eq)

data FilesEntryObj = FilesEntryObj
    { entryName :: Text
    , entryType :: Int
    , entrySize :: Int
    , entryHash :: Text
    }
    deriving (Show, Eq)

data FilesLsObj = FilesLsObj
    { enteries :: [FilesEntryObj]
    }
    deriving (Show, Eq)

data FilesFlushObj = FilesFlushObj
    { fileCid :: Text
    }
    deriving (Show, Eq)

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
                      Nothing  -> mzero

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
                      Nothing  -> mzero

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

instance FromJSON FilesEntryObj where
    parseJSON (Object o) =
        FilesEntryObj  <$> o .: "Name"
                       <*> o .: "Type"
                       <*> o .: "Size"
                       <*> o .: "Hash"

    parseJSON _ = mzero

instance FromJSON FilesLsObj where
    parseJSON (Object o) =
        FilesLsObj  <$> o .: "Entries"

    parseJSON _ = mzero

instance FromJSON FilesFlushObj where
    parseJSON (Object o) =
        FilesFlushObj  <$> o .: "Cid"

    parseJSON _ = mzero

-- | Defining a content type same as PlainText without charset
data IpfsText deriving Typeable

instance Servant.API.Accept IpfsText where
    contentType _ = "text" M.// "plain"

-- | @left show . TextS.decodeUtf8' . toStrict@
instance MimeUnrender IpfsText Text where
    mimeUnrender _ = left show . TextS.decodeUtf8' . toStrict


-- | Defining a content type same as IpfsJSON
data IpfsJSON deriving Typeable

instance Servant.API.Accept IpfsJSON where
    contentType _ = "application" M.// "json"

-- | @left show . TextS.decodeUtf8' . toStrict@
instance MimeUnrender IpfsJSON Text where
    mimeUnrender _ = left show . TextS.decodeUtf8' . toStrict

type IpfsApi = "cat" :> Capture "arg" Text :> Get '[IpfsText] CatReturnType
            :<|> "ls" :> Capture "arg" Text :> Get '[JSON] LsObj
            :<|> "get" :> Capture "arg" Text :> Get '[IpfsText] GetReturnType
            :<|> "swarm" :> "peers" :> Get '[JSON] SwarmPeersObj
            :<|> "swarm" :> "connect" :> QueryParam "arg" Text :> Get '[JSON] SwarmObj
            :<|> "swarm" :> "disconnect" :> QueryParam "arg" Text :> Get '[JSON] SwarmObj
            :<|> "swarm" :> "filters" :> Get '[JSON] SwarmObj
            :<|> "swarm" :> "filters" :> "add" :> QueryParam "arg" Text :> Get '[JSON] SwarmObj
            :<|> "swarm" :> "filters" :> "rm" :> QueryParam "arg" Text :> Get '[JSON] SwarmObj
            :<|> "bitswap" :> "stat" :> Get '[JSON] BitswapStatObj
            :<|> "bitswap" :> "wantlist" :> Get '[JSON] BitswapWLObj
            :<|> "bitswap" :> "ledger" :> Capture "peerId" Text :> Get '[JSON] BitswapLedgerObj
            :<|> "bitswap" :> "reprovide" :> Get '[IpfsText] ReprovideReturnType
            :<|> "cid" :> "bases" :> Get '[JSON] [CidBasesObj]
            :<|> "cid" :> "codecs" :> Get '[JSON] [CidCodecsObj]
            :<|> "cid" :> "hashes" :> Get '[JSON] [CidHashesObj]
            :<|> "cid" :> "base32" :> Capture "cid" Text :> Get '[JSON] CidObj
            :<|> "cid" :> "format" :> Capture "cid" Text :> Get '[JSON] CidObj
            :<|> "block" :> "get" :> Capture "key" Text :> Get '[IpfsText] BlockReturnType
            :<|> "block" :> "stat" :> Capture "key" Text :> Get '[JSON] BlockObj
            :<|> "dag" :> "get" :> Capture "ref" Text :> Get '[IpfsJSON] DagReturnType
            :<|> "dag" :> "resolve" :> Capture "ref" Text :> Get '[JSON] DagResolveObj
            :<|> "config" :> Capture "ref" Text :> Get '[JSON] ConfigObj
            :<|> "config" :> Capture "arg" Text :> QueryParam "arg" Text :> Get '[JSON] ConfigObj
            :<|> "object" :> "data" :> Capture "ref" Text :> Get '[IpfsText] ObjectReturnType
            :<|> "object" :> "new" :> Get '[JSON] ObjectObj
            :<|> "object" :> "links" :>  Capture "ref" Text :> Get '[JSON] ObjectLinksObj
            :<|> "object" :> "patch" :> "add-link" :> Capture "arg" Text
                :> QueryParam "arg" Text :> QueryParam "arg" Text :> Get '[JSON] ObjectLinksObj
            :<|> "object" :> "patch" :> "rm-link" :> Capture "arg" Text
                :> QueryParam "arg" Text :> Get '[JSON] ObjectLinksObj
            :<|> "object" :> "get" :> Capture "arg" Text :> Get '[JSON] ObjectGetObj
            :<|> "object" :> "diff" :> Capture "arg" Text :> QueryParam "arg" Text :> Get '[JSON] ObjectDiffObj
            :<|> "object" :> "stat" :> Capture "arg" Text :> Get '[JSON] ObjectStatObj
            :<|> "pin" :> "add" :> Capture "arg" Text :> Get '[JSON] PinObj
            :<|> "pin" :> "rm" :> Capture "arg" Text :> Get '[JSON] PinObj
            :<|> "bootstrap" :> "add" :> QueryParam "arg" Text :> Get '[JSON] BootstrapObj
            :<|> "bootstrap" :> "list" :> Get '[JSON] BootstrapObj
            :<|> "bootstrap" :> "rm" :> QueryParam "arg" Text :> Get '[JSON] BootstrapObj
            :<|> "stats" :> "bw" :> Get '[JSON] StatsBwObj
            :<|> "stats" :> "repo" :> Get '[JSON] StatsRepoObj
            :<|> "version" :> Get '[JSON] VersionObj
            :<|> "id" :> Get '[JSON] IdObj
            :<|> "id" :> Capture "arg" Text :> Get '[JSON] IdObj
            :<|> "dns" :> Capture "arg" Text :> Get '[JSON] DnsObj
            :<|> "pubsub" :> "ls" :>  Get '[JSON] PubsubObj
            :<|> "pubsub" :> "peers" :>  Get '[JSON] PubsubObj
            :<|> "pubsub" :> "pub" :> Capture "arg" Text :> QueryParam "arg" Text :> Get '[JSON] NoContent
            :<|> "log" :> "ls" :>  Get '[JSON] LogLsObj
            :<|> "log" :> "level" :> Capture "arg" Text :> QueryParam "arg" Text :> Get '[JSON] LogLevelObj
            :<|> "repo" :> "version" :>  Get '[JSON] RepoVersionObj
            :<|> "repo" :> "fsck" :>  Get '[JSON] RepoFsckObj
            :<|> "key" :> "gen" :> Capture "arg" Text :> QueryParam "type" Text :> Get '[JSON] KeyDetailsObj
            :<|> "key" :> "list" :>  Get '[JSON] KeyObj
            :<|> "key" :> "rename" :> Capture "arg" Text :> QueryParam "arg" Text :> Get '[JSON] KeyRenameObj
            :<|> "key" :> "rm" :> Capture "arg" Text :> Get '[JSON] KeyObj
            :<|> "files" :> "chcid" :> QueryParam "arg" Text :> QueryParam "cid-version" Int :> Get '[JSON] NoContent
            :<|> "files" :> "cp" :> QueryParam "arg" Text :> QueryParam "arg" Text :> Get '[JSON] NoContent
            :<|> "files" :> "flush" :> QueryParam "arg" Text :> Get '[JSON] FilesFlushObj
            :<|> "files" :> "ls" :> QueryParam "arg" Text :> Get '[JSON] FilesLsObj
            :<|> "files" :> "mkdir" :> QueryParam "arg" Text :> Get '[JSON] NoContent
            :<|> "files" :> "mv" :> QueryParam "arg" Text :> QueryParam "arg" Text :> Get '[JSON] NoContent
            :<|> "files" :> "read" :> QueryParam "arg" Text :> Get '[IpfsText] FilesReadType
            :<|> "files" :> "rm" :> QueryParam "arg" Text :> QueryParam "recursive" Bool :> Get '[JSON] NoContent
            :<|> "files" :> "stat" :> QueryParam "arg" Text :> Get '[JSON] FilesStatObj
            :<|> "shutdown" :> Get '[JSON] NoContent
