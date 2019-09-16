{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      :  Network.Ipfs.Api.Ipfs
-- Copyright   :  Alexander Krupenkin 2016-2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unknown
--
-- Module containing Ipfs command functions.
--

module Network.Ipfs.Api.Ipfs where

import qualified Codec.Archive.Tar                      as Tar
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson                             (decode)
import           Data.Text
import           Data.Text.Encoding
import           Data.Base58String.Bitcoin              (fromBytes, toText)
import qualified Data.ByteString                        as BS'(ByteString, foldr)
import qualified Data.ByteArray.Encoding                as Enc(convertFromBase, Base(..))
import qualified Data.ByteString.Lazy                   as BS (ByteString, fromStrict) 
import           Network.HTTP.Client                    as Net  hiding (Proxy)
import           Network.HTTP.Client.MultipartFormData
import           Network.HTTP.Types                     (Status(..))
import           Numeric                                (showInt)
import           Servant.Client
import           Servant.Types.SourceT                  (SourceT(..), foreach)
import qualified Servant.Client.Streaming               as S

import qualified Network.Ipfs.Api.Api                   as Api
import           Network.Ipfs.Api.Multipart             (AddObj)
import           Network.Ipfs.Api.Stream                (_ping, _dhtFindPeer, _dhtFindProvs, _dhtGet, _dhtProvide,
                                                        _dhtQuery, _logTail, _repoGc, _repoVerify, _refs, _refsLocal,
                                                        _pubsubSubscribe, PubsubSubObj(..))

newtype IpfsT m a = IpfsT { unIpfs :: ReaderT (Manager, BaseUrl, String) (ExceptT ServantError m) a}
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader (Manager, BaseUrl, String)
           , MonadError ServantError
           )

instance MonadTrans IpfsT where
  lift = IpfsT . lift . lift

type Ipfs a = IpfsT IO a

------------------------------------------- Monad Runners ---------------------------------------------------

-- | 'IpfsT' monad runner.
runIpfs' :: BaseUrl -> Ipfs a -> IO ()
runIpfs' url ipfs = do
  manager' <- liftIO $ newManager defaultManagerSettings
  ret <- runExceptT (runReaderT (unIpfs ipfs) (manager', url, showBaseUrl url))
  case ret of
    Left err -> putStrLn $ "Error: " ++ show err
    Right _ -> putStr ""

-- | 'IpfsT' monad runner with default arguments.
runIpfs :: Ipfs a -> IO ()
runIpfs = runIpfs' (BaseUrl Http "localhost" 5001 "/api/v0")


------------------------------------------- Call functions ---------------------------------------------------

-- | Regular Call function.
call :: (ClientM a) -> Ipfs a
call func = do
  (manager', url, _) <- ask
  resp <- lift (runClientM func (mkClientEnv manager' url))
  case resp of
    Left l -> throwError l
    Right r -> pure r

-- | Call function for Streams. 
streamCall :: Show a => S.ClientM (SourceT IO a) -> IO()
streamCall func = do 
    manager' <- newManager defaultManagerSettings
    S.withClientM func (S.mkClientEnv manager' (BaseUrl Http "localhost" 5001 "/api/v0")) $ \e -> case e of
        Left err -> putStrLn $ "Error: " ++ show err
        Right rs -> foreach fail print rs

-- | Call function for 'PubsubSubObj'. 
pubsubCall :: S.ClientM (SourceT IO PubsubSubObj) -> IO()
pubsubCall func = do 
    manager' <- newManager defaultManagerSettings
    S.withClientM func (S.mkClientEnv manager' (BaseUrl Http "localhost" 5001 "/api/v0")) $ \e -> case e of
        Left err -> putStrLn $ "Error: " ++ show err
        Right rs -> foreach fail printPubsub rs

-- | Call function for ‘multipart/form-data’. 
multipartCall :: Text -> Text -> Ipfs (Net.Response BS.ByteString)
multipartCall funcUri filePath = do
    (reqManager, _, url) <- ask
    req <- liftIO $ parseRequest $ unpack (( pack  url ) <>  (pack "/") <> funcUri )
    resp <- liftIO $ flip httpLbs reqManager =<< formDataBody form req
    pure resp
    
    where form = [ partFileSource "file" $ unpack filePath ]


------------------------------------------- Print Functions ---------------------------------------------------

-- | Print function for the Base64 decoded 'PubsubSubObj'.
printPubsub :: PubsubSubObj -> IO ()
printPubsub PubsubSubObj {mssgdata = mssg, from = sender, seqno = num, topicIDs = topic } = 
    print $ PubsubSubObj (fromB64 mssg) (fromB64toB58 sender) (fromB64' num) topic
    where fromB64toB58 val = case Enc.convertFromBase Enc.Base64 (encodeUtf8 val) of
                                Left e -> pack $ "Invalid input: " ++ e
                                Right decoded -> toText $ fromBytes (decoded :: BS'.ByteString)
          
          fromB64 val = case Enc.convertFromBase Enc.Base64 (encodeUtf8 val) of
                            Left e -> pack $ "Invalid input: " ++ e
                            Right decoded -> decodeUtf8 (decoded :: BS'.ByteString)

          fromB64' val = case Enc.convertFromBase Enc.Base64 (encodeUtf8 val) of
                            Left e -> pack $ "Invalid input: " ++ e
                            Right decoded -> pack $ BS'.foldr showInt "" (decoded :: BS'.ByteString)


------------------------------------------- Ipfs functions ---------------------------------------------------

-- | Show IPFS object data. 
cat :: Text -> Ipfs Api.CatReturnType
cat hash = call $ Api._cat hash

-- | Add a file or directory to ipfs. 
add :: Text -> Ipfs (Maybe AddObj)
add filePath =  do 
    responseVal <- ( multipartCall (pack "add") filePath )
    pure (decode (Net.responseBody responseVal)  :: Maybe AddObj)
        
-- | List directory contents for Unix filesystem objects. 
ls :: Text -> Ipfs Api.LsObj
ls hash = call $ Api._ls hash

-- | Download IPFS objects. 
get :: Text -> Ipfs Text
get hash = do 
    ret <- call $ Api._get hash
    do liftIO $ Tar.unpack "getResponseDirectory" . Tar.read $ BS.fromStrict $ encodeUtf8 ret
       pure "The content has been stored in getResponseDirectory."

-- | List links (references) from an object. Stream function, returns IO(), use liftIO while passing to 'runIpfs' or 'runIpfs''.
refs :: Text -> IO ()
refs hash = streamCall $ _refs hash

-- | List all local references. Stream function, returns IO(), use liftIO while passing to 'runIpfs' or 'runIpfs''. 
refsLocal :: IO ()
refsLocal = streamCall _refsLocal

-- | List peers with open connections. 
swarmPeers :: Ipfs Api.SwarmPeersObj
swarmPeers = call Api._swarmPeers       

-- | Open connection to a given address. 'peerId' has to be of the format - /ipfs/id 
swarmConnect :: Text -> Ipfs Api.SwarmObj
swarmConnect peerId = call $ Api._swarmConnect (Just peerId)  


-- | Close connection to a given address. 'peerId' has to be of the format - /ipfs/id 
swarmDisconnect :: Text -> Ipfs Api.SwarmObj
swarmDisconnect peerId = call $ Api._swarmDisconnect (Just peerId)  

-- | Manipulate address filters. 
swarmFilters :: Ipfs Api.SwarmObj
swarmFilters = call Api._swarmFilters

-- | Add an address filter. 'peerId' has to be of the format - /ip4/{IP addr of peer}/ipcidr/{ip network prefix} 
swarmFilterAdd :: Text -> Ipfs Api.SwarmObj
swarmFilterAdd filterParam = call $ Api._swarmFilterAdd (Just filterParam)  

-- | Remove an address filter. 
swarmFilterRm :: Text -> Ipfs Api.SwarmObj
swarmFilterRm filterParam = call $ Api._swarmFilterRm (Just filterParam)  

-- | 'Show some diagnostic information on the bitswap agent. 
bitswapStat :: Ipfs Api.BitswapStatObj
bitswapStat = call Api._bitswapStat
        
-- | Show blocks currently on the wantlist. 
bitswapWL :: Ipfs Api.BitswapWLObj
bitswapWL = call Api._bitswapWL 
        
-- | Show the current ledger for a peer. 
bitswapLedger :: Text -> Ipfs Api.BitswapLedgerObj
bitswapLedger peerId = call $ Api._bitswapLedger peerId
        
-- | Trigger reprovider. 
bitswapReprovide :: Ipfs Api.ReprovideReturnType
bitswapReprovide = call $ Api._bitswapReprovide

-- | List available multibase encodings. 
cidBases :: Ipfs [Api.CidBasesObj]
cidBases = call $ Api._cidBases
        
-- | List available CID codecs. 
cidCodecs :: Ipfs [Api.CidCodecsObj]
cidCodecs = call $ Api._cidCodecs
        
-- | List available multihashes. 
cidHashes :: Ipfs [Api.CidHashesObj]
cidHashes = call $ Api._cidHashes

-- | Convert CIDs to Base32 CID version 1. 
cidBase32 :: Text -> Ipfs Api.CidObj
cidBase32 hash = call $ Api._cidBase32 hash
                
-- | Format and convert a CID in various useful ways. 
cidFormat :: Text-> Ipfs Api.CidObj
cidFormat hash = call $ Api._cidFormat hash
        
-- | Get a raw IPFS block. 
blockGet :: Text -> Ipfs Api.BlockReturnType
blockGet key = call $ Api._blockGet key
        
-- | Store input as an IPFS block. 
blockPut :: Text -> Ipfs (Maybe Api.BlockObj)
blockPut filePath = do 
    responseVal <- multipartCall (pack "block/put") filePath 
    pure (decode (Net.responseBody responseVal)  :: Maybe Api.BlockObj)

-- | Print information of a raw IPFS block. 
blockStat :: Text -> Ipfs Api.BlockObj
blockStat key = call $ Api._blockStat key

-- | Get a dag node from ipfs. 
dagGet :: Text -> Ipfs Api.DagReturnType
dagGet ref = call $ Api._dagGet ref

-- | Resolve ipld block. 
dagResolve :: Text -> Ipfs Api.DagResolveObj
dagResolve ref = call $ Api._dagResolve ref

-- | Add a dag node to ipfs. 
dagPut :: Text -> Ipfs (Maybe Api.DagPutObj)
dagPut filePath = do 
    responseVal <- multipartCall (pack "dag/put") filePath 
    pure (decode (Net.responseBody responseVal)  :: Maybe Api.DagPutObj)

-- | Get ipfs config values. 
configGet :: Text -> Ipfs Api.ConfigObj
configGet key = call $ Api._configGet key

-- | Set ipfs config values. 
configSet :: Text -> Text -> Ipfs Api.ConfigObj
configSet key value = call $ Api._configSet key $ Just value

-- | Replace the config with the file at <filePath>. 
configReplace :: Text -> Ipfs (Maybe Text)
configReplace filePath = do 
    responseVal <- multipartCall (pack "config/replace") filePath 
    pure $ case statusCode $ Net.responseStatus responseVal of 
            200 -> Just $ "Config File Replaced Successfully with status code - " <> (pack "200")
            _   -> Just $ "Error occured with status code - " <>  (pack $ show (statusCode $ Net.responseStatus responseVal))
                
-- | Output the raw bytes of an IPFS object. 
objectData :: Text -> Ipfs Api.ObjectReturnType
objectData key = call $ Api._objectData key

-- | Create a new object from an ipfs template. 
objectNew :: Ipfs Api.ObjectObj
objectNew = call Api._objectNew
 
-- | Output the links pointed to by the specified object. 
objectGetLinks :: Text -> Ipfs Api.ObjectLinksObj
objectGetLinks key = call $ Api._objectGetLinks key

-- | Add a Merkle-link to the given object and return the hash of the result. 
objectAddLink ::  Text -> Text -> Text -> Ipfs Api.ObjectLinksObj
objectAddLink hash name key = call $ Api._objectAddLink hash (Just name) (Just key)

-- | Remove a Merkle-link from the given object and return the hash of the result. 
objectRmLink :: Text -> Text -> Ipfs Api.ObjectLinksObj
objectRmLink key name = call $ Api._objectRmLink key (Just name)

-- | Append data to what already exists in the data segment in the given object. 
objectAppendData :: Text -> Text -> Ipfs (Maybe Api.ObjectLinksObj)
objectAppendData key filePath = do 
    responseVal <- multipartCall ( ( pack "object/patch/append-data?arg=" ) <> key) filePath 
    pure ( decode ( Net.responseBody responseVal)  :: Maybe Api.ObjectLinksObj ) 

-- | Set the data field of an IPFS object. 
objectSetData :: Text -> Text -> Ipfs (Maybe Api.ObjectLinksObj)
objectSetData key filePath = do 
    responseVal <- multipartCall ( ( pack "object/patch/set-data?arg=" ) <> key) filePath 
    pure ( decode ( Net.responseBody responseVal)  :: Maybe Api.ObjectLinksObj )      

-- | Get and serialize the DAG node named by key. 
objectGet :: Text -> Ipfs Api.ObjectGetObj
objectGet key = call $ Api._objectGet key

-- | 'Display the diff between two ipfs objects. 
objectDiff :: Text -> Text -> Ipfs Api.ObjectDiffObj
objectDiff firstKey secondKey = call $ Api._objectDiff firstKey (Just secondKey)

-- | Store input as a DAG object, print its key. 
objectPut :: Text -> Ipfs ( Maybe Api.ObjectObj )
objectPut filePath = do 
    responseVal <- multipartCall (pack "object/put") filePath 
    pure (decode ( Net.responseBody responseVal)  :: Maybe Api.ObjectObj)        

    -- | Get stats for the DAG node named by key. 
objectStat :: Text -> Ipfs Api.ObjectStatObj
objectStat key = call $ Api._objectStat key

-- | Pin objects to local storage. 
pinAdd :: Text -> Ipfs Api.PinObj
pinAdd pinPath = call $ Api._pinAdd pinPath

-- | Remove pinned objects from local storage. 
pinRemove :: Text -> Ipfs Api.PinObj
pinRemove pinPath = call $ Api._pinRemove pinPath

-- | Add peers to the bootstrap list. 
bootstrapAdd :: Text -> Ipfs Api.BootstrapObj
bootstrapAdd peerId = call $ Api._bootstrapAdd (Just peerId)

-- | Show peers in the bootstrap list. 
bootstrapList :: Ipfs Api.BootstrapObj
bootstrapList = call $ Api._bootstrapList

-- | Remove peers from the bootstrap list. 
bootstrapRM :: Text -> Ipfs Api.BootstrapObj
bootstrapRM peerId = call $ Api._bootstrapRM  (Just peerId)

-- | Print ipfs bandwidth information. 
statsBw :: Ipfs Api.StatsBwObj
statsBw = call Api._statsBw  

-- | Get stats for the currently used repo. 
statsRepo :: Ipfs Api.StatsRepoObj
statsRepo = call $ Api._statsRepo  

-- | Show ipfs version information. 
version :: Ipfs Api.VersionObj
version = call $ Api._version  

-- | Show ipfs node id info. 
id :: Ipfs Api.IdObj
id = call Api._id  

-- | Show ipfs node id info of the given peerId. 
idPeer :: Text -> Ipfs Api.IdObj
idPeer peerId = call $ Api._idPeer peerId  

-- | Resolve DNS links. 
dns :: Text -> Ipfs Api.DnsObj
dns name = call $ Api._dns name  

-- | Send echo request packets to IPFS hosts. Stream function, returns IO(), use liftIO while passing to 'runIpfs' or 'runIpfs''. 
ping :: Text -> IO ()
ping cid = streamCall $ _ping cid  

-- | Find the multiaddresses associated with the given peerId. Stream function, returns IO(), use liftIO while passing to 'runIpfs' or 'runIpfs''. 
dhtFindPeer :: Text -> IO ()
dhtFindPeer peerId = streamCall $ _dhtFindPeer peerId  

-- | Find peers that can provide a specific value, given a key. Stream function, returns IO(), use liftIO while passing to 'runIpfs' or 'runIpfs''. 
dhtFindProvs :: Text -> IO ()
dhtFindProvs cid = streamCall $ _dhtFindProvs cid  

-- | 'Given a key, query the routing system for its best value. Stream function, returns IO(), use liftIO while passing to 'runIpfs' or 'runIpfs''. 
dhtGet :: Text -> IO ()
dhtGet cid = streamCall $ _dhtGet cid  

-- | 'Announce to the network that you are providing given values. Stream function, returns IO(), use liftIO while passing to 'runIpfs' or 'runIpfs''. 
dhtProvide :: Text -> IO ()
dhtProvide cid = streamCall $ _dhtProvide cid 

-- | Find the closest Peer IDs to a given peerID by querying the DHT. Stream function, returns IO(), use liftIO while passing to 'runIpfs' or 'runIpfs''. 
dhtQuery ::  Text -> IO ()
dhtQuery peerId = streamCall $ _dhtQuery peerId

-- | List subscribed topics by name. 
pubsubLs :: Ipfs Api.PubsubObj
pubsubLs = call Api._pubsubLs  

-- | List peers we are currently pubsubbing with. 
pubsubPeers :: Ipfs Api.PubsubObj
pubsubPeers = call Api._pubsubPeers
--}
-- | Publish a message to a given pubsub topic.
pubsubPublish :: Text -> Text -> Ipfs Text
pubsubPublish topic mssg = do 
    call $ Api._pubsubPublish topic $ Just mssg
    pure "The given message has been published."
     
-- | Subscribe to messages on a given topic. Stream function, returns IO(), use liftIO while passing to 'runIpfs' or 'runIpfs''.
pubsubSubscribe :: Text -> IO ()
pubsubSubscribe topic = pubsubCall $ _pubsubSubscribe topic

-- | List the logging subsystems. 
logLs :: Ipfs Api.LogLsObj
logLs = call Api._logLs

-- | Change the logging level. 
logLevel :: Text -> Text -> Ipfs Api.LogLevelObj
logLevel subsystem level = call $ Api._logLevel subsystem $ Just level

-- | Read the event log. Stream function, returns IO(), use liftIO while passing to 'runIpfs' or 'runIpfs''.
logTail :: IO ()
logTail = streamCall _logTail

-- | Show the repo version. 
repoVersion :: Ipfs Api.RepoVersionObj
repoVersion = call Api._repoVersion

-- | Remove repo lockfiles. 
repoFsck :: Ipfs Api.RepoFsckObj
repoFsck = call Api._repoFsck

-- | Perform a garbage collection sweep on the repo. Stream function, returns IO(), use liftIO while passing to 'runIpfs' or 'runIpfs''.
repoGc :: IO ()
repoGc = streamCall _repoGc

-- | Verify all blocks in repo are not corrupted. Stream function, returns IO(), use liftIO while passing to 'runIpfs' or 'runIpfs''. 
repoVerify :: IO ()
repoVerify = streamCall _repoVerify

-- | 'List all local keypairs. 
keyList :: Ipfs Api.KeyObj
keyList = call Api._keyList

-- | Create a new keypair. 
keyGen :: Text -> Text -> Ipfs Api.KeyDetailsObj
keyGen name keyType = call $ Api._keyGen name (Just keyType)

-- | Rename a keypair. 
keyRename :: Text -> Text -> Ipfs Api.KeyRenameObj
keyRename was now  = call $ Api._keyRename was $ Just now       

-- | Remove a keypair. 
keyRm :: Text -> Ipfs Api.KeyObj
keyRm name  = call $ Api._keyRm name 

-- | Change the cid version or hash function of the root node of a given mfsPath. 
filesChcidVer :: Text -> Int -> Ipfs Text
filesChcidVer mfsPath cidVersion = do 
    call $ Api._filesChcid (Just mfsPath) (Just cidVersion)
    pure "The directory's cid version has been changed."

-- | Copy files into mfs. 
filesCp :: Text -> Text -> Ipfs Text
filesCp src dest  = do 
    call $ Api._filesCp (Just src) (Just dest)
    pure "The object has been copied to the specified destination"

-- | Flush a given path's data to disk. 
filesFlush ::Text -> Ipfs Api.FilesFlushObj
filesFlush mfsPath = call $ Api._filesFlush $ Just mfsPath 

-- | List directories in the local mutable namespace. 
filesLs :: Text -> Ipfs Api.FilesLsObj
filesLs mfsPath = call $ Api._filesLs $ Just mfsPath 

-- | Make directories. 
filesMkdir :: Text -> Ipfs Text
filesMkdir mfsPath  = do 
    call $ Api._filesMkdir $ Just mfsPath 
    pure "The Directory has been created on the specified path."

-- | Move files. 
filesMv :: Text -> Text -> Ipfs Text
filesMv src dest  = do 
    call $ Api._filesMv (Just src) (Just dest)
    pure "The object has been moved to the specified destination"

-- | Read a file in a given mfs. 
filesRead :: Text -> Ipfs Api.FilesReadType
filesRead mfsPath  = call $ Api._filesRead $ Just mfsPath 

-- | Display file status. 
filesStat :: Text -> Ipfs Api.FilesStatObj
filesStat mfsPath  = call $ Api._filesStat $ Just mfsPath

-- | Remove a file. 
filesRm :: Text -> Ipfs Text
filesRm mfsPath  = do 
    call $ Api._filesRm (Just mfsPath) (Just True)  
    pure "The object has been removed."

-- | Write to a mutable file in a given filesystem. 
filesWrite :: Text -> Text -> Bool -> Ipfs (Maybe Text)
filesWrite mfsPath filePath toTruncate = do 
    responseVal <- multipartCall ((pack "files/write?arg=") 
        <> mfsPath <> (pack "&create=true") <>  (pack "&truncate=") <> (pack $ show toTruncate) ) filePath 
    pure $ case statusCode $ Net.responseStatus responseVal of 
            200 -> Just $ "File has been written Successfully with status code - " <> (pack "200")
            _   -> Just $ "Error occured with status code - " <>  (pack $ show (statusCode $ Net.responseStatus responseVal))

-- | Shut down the ipfs daemon. 
shutdown :: Ipfs Text
shutdown = do 
    call $ Api._shutdown   
    pure "The daemon has been shutdown, your welcome."
