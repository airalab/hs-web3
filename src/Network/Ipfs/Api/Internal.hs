-- |
-- Module      :  Network.Ipfs.Api.Internal
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- IPFS API internals.
--

module Network.Ipfs.Api.Internal where

import           Data.Proxy             (Proxy (..))
import           Data.Text              (Text)
import           Servant.API
import           Servant.Client         (ClientM, client)

import           Network.Ipfs.Api.Types

_ipfsApi :: Proxy IpfsApi
_ipfsApi = Proxy

_cat :: Text -> ClientM CatReturnType
_ls :: Text -> ClientM LsObj
_get :: Text -> ClientM GetReturnType
_swarmPeers :: ClientM SwarmPeersObj
_swarmConnect :: Maybe Text -> ClientM SwarmObj
_swarmDisconnect :: Maybe Text -> ClientM SwarmObj
_swarmFilters :: ClientM SwarmObj
_swarmFilterAdd :: Maybe Text -> ClientM SwarmObj
_swarmFilterRm :: Maybe Text -> ClientM SwarmObj
_bitswapStat :: ClientM BitswapStatObj
_bitswapWL :: ClientM BitswapWLObj
_bitswapLedger :: Text -> ClientM BitswapLedgerObj
_bitswapReprovide :: ClientM ReprovideReturnType
_cidBases :: ClientM [CidBasesObj]
_cidCodecs :: ClientM [CidCodecsObj]
_cidHashes :: ClientM [CidHashesObj]
_cidBase32 :: Text -> ClientM CidObj
_cidFormat :: Text -> ClientM CidObj
_blockGet :: Text -> ClientM BlockReturnType
_blockStat :: Text -> ClientM BlockObj
_dagGet :: Text -> ClientM DagReturnType
_dagResolve :: Text -> ClientM DagResolveObj
_configGet :: Text -> ClientM ConfigObj
_configSet :: Text -> Maybe Text -> ClientM ConfigObj
_objectData :: Text -> ClientM ObjectReturnType
_objectNew :: ClientM ObjectObj
_objectGetLinks :: Text -> ClientM ObjectLinksObj
_objectAddLink :: Text -> Maybe Text -> Maybe Text -> ClientM ObjectLinksObj
_objectRmLink :: Text -> Maybe Text -> ClientM ObjectLinksObj
_objectGet :: Text -> ClientM ObjectGetObj
_objectDiff :: Text -> Maybe Text -> ClientM ObjectDiffObj
_objectStat :: Text -> ClientM ObjectStatObj
_pinAdd :: Text -> ClientM PinObj
_pinRemove :: Text -> ClientM PinObj
_bootstrapAdd :: Maybe Text -> ClientM BootstrapObj
_bootstrapList :: ClientM BootstrapObj
_bootstrapRM :: Maybe Text -> ClientM BootstrapObj
_statsBw :: ClientM StatsBwObj
_statsRepo :: ClientM StatsRepoObj
_version :: ClientM VersionObj
_id :: ClientM IdObj
_idPeer :: Text -> ClientM IdObj
_dns :: Text -> ClientM DnsObj
_pubsubLs :: ClientM PubsubObj
_pubsubPeers :: ClientM PubsubObj
_pubsubPublish :: Text -> Maybe Text -> ClientM NoContent
_logLs :: ClientM LogLsObj
_logLevel :: Text -> Maybe Text -> ClientM LogLevelObj
_repoVersion :: ClientM RepoVersionObj
_repoFsck :: ClientM RepoFsckObj
_keyGen :: Text -> (Maybe Text) -> ClientM KeyDetailsObj
_keyList :: ClientM KeyObj
_keyRename :: Text -> (Maybe Text) -> ClientM KeyRenameObj
_keyRm :: Text -> ClientM KeyObj
_filesChcid :: Maybe Text -> Maybe Int -> ClientM NoContent
_filesCp :: Maybe Text -> Maybe Text -> ClientM NoContent
_filesFlush :: Maybe Text -> ClientM FilesFlushObj
_filesLs :: Maybe Text -> ClientM FilesLsObj
_filesMkdir :: Maybe Text -> ClientM NoContent
_filesMv :: Maybe Text -> Maybe Text -> ClientM NoContent
_filesRead :: Maybe Text -> ClientM FilesReadType
_filesRm :: Maybe Text -> Maybe Bool -> ClientM NoContent
_filesStat :: Maybe Text -> ClientM FilesStatObj
_shutdown :: ClientM NoContent

_cat :<|> _ls :<|> _get :<|> _swarmPeers :<|> _swarmConnect :<|> _swarmDisconnect :<|>
  _swarmFilters :<|> _swarmFilterAdd :<|> _swarmFilterRm :<|>  _bitswapStat :<|> _bitswapWL :<|> _bitswapLedger :<|>
  _bitswapReprovide :<|> _cidBases :<|> _cidCodecs :<|> _cidHashes :<|> _cidBase32 :<|> _cidFormat :<|>
  _blockGet  :<|> _blockStat :<|> _dagGet :<|> _dagResolve :<|> _configGet :<|>
  _configSet :<|> _objectData :<|> _objectNew :<|> _objectGetLinks :<|> _objectAddLink :<|> _objectRmLink :<|>
  _objectGet :<|> _objectDiff :<|> _objectStat :<|> _pinAdd :<|> _pinRemove :<|> _bootstrapAdd :<|>
  _bootstrapList :<|> _bootstrapRM :<|> _statsBw :<|> _statsRepo :<|> _version :<|> _id :<|> _idPeer :<|>
  _dns :<|> _pubsubLs :<|> _pubsubPeers :<|> _pubsubPublish :<|> _logLs :<|> _logLevel :<|> _repoVersion :<|>
  _repoFsck :<|> _keyGen :<|> _keyList :<|> _keyRename :<|> _keyRm :<|> _filesChcid :<|> _filesCp :<|>
  _filesFlush :<|> _filesLs :<|> _filesMkdir :<|> _filesMv :<|> _filesRead :<|> _filesRm :<|> _filesStat :<|>
  _shutdown = client _ipfsApi
