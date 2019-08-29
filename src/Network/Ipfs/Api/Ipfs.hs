{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import           Data.Aeson                             (decode)
import           Data.Text                              as TextS
import qualified Data.Text.Encoding                     as TextS
import qualified Data.Text.IO                           as TextIO
import qualified Data.ByteString.Lazy                   as BS (ByteString, fromStrict) 
import           Network.HTTP.Client                    as Net  hiding (Proxy)
import           Network.HTTP.Client.MultipartFormData
import           Network.HTTP.Types                     (Status(..))
import           Servant.Client
import qualified Servant.Client.Streaming               as S
import           Servant.Types.SourceT                  (SourceT, foreach)

import           Network.Ipfs.Api.Api                   (_cat, _ls, _get, _swarmPeers, _swarmConnect,
                                                        _swarmDisconnect, _swarmFilterAdd, _swarmFilters,
                                                        _swarmFilterRm, _bitswapStat, _bitswapWL, _bitswapLedger,
                                                        _bitswapReprovide, _cidBases, _cidCodecs, _cidHashes, _cidBase32,
                                                        _cidFormat, _blockGet, _objectDiff, _blockStat, _dagGet,
                                                        _dagResolve, _configGet, _configSet, _objectData,
                                                        _objectNew, _objectGetLinks, _objectAddLink, _objectRmLink,
                                                        _objectGet, _objectStat, _pinAdd, _pinRemove,_bootstrapList, 
                                                        _bootstrapAdd, _bootstrapRM, _statsBw, _statsRepo, _version,
                                                        _id, _idPeer, _dns, _pubsubLs, _pubsubPeers, _pubsubPublish, _logLs, _logLevel,
                                                        _repoVersion, _repoFsck, _keyGen, _keyList, _keyRm, _keyRename,
                                                        _filesChcid, _filesCp, _filesFlush, _filesLs, _filesMkdir, 
                                                        _filesMv, _filesRead, _filesRm, _filesStat, _shutdown,
                                                        BlockObj, DagPutObj, ObjectObj, ObjectLinksObj, KeyDetailsObj, KeyRenameObj, KeyObj)

import           Network.Ipfs.Api.Multipart   (AddObj)
import           Network.Ipfs.Api.Stream      (_ping, _dhtFindPeer, _dhtFindProvs, _dhtGet, _dhtProvide,
                                              _dhtQuery, _logTail, _repoGc, _repoVerify, _refs, _refsLocal, _pubsubSubscribe)

-- | Regular Call function
call :: ClientM a -> IO (Either ServantError a)
call func = do 
    manager' <- newManager defaultManagerSettings
    runClientM func (mkClientEnv manager' (BaseUrl Http "localhost" 5001 "/api/v0"))

-- | Call function for Streams. 
streamCall :: Show a => S.ClientM (SourceT IO a) -> IO()
streamCall func = do 
    manager' <- newManager defaultManagerSettings
    S.withClientM func (S.mkClientEnv manager' (BaseUrl Http "localhost" 5001 "/api/v0")) $ \e -> case e of
        Left err -> putStrLn $ "Error: " ++ show err
        Right rs -> foreach fail print rs

-- | Call function for ‘multipart/form-data’. 
multipartCall ::  Text -> Text -> IO (Net.Response BS.ByteString)
multipartCall uri filePath = do
    reqManager <- newManager defaultManagerSettings
    req <- parseRequest $ TextS.unpack uri
    resp <- flip httpLbs reqManager =<< formDataBody form req
    return (resp)
    
    where form = [ partFileSource "file" $ TextS.unpack filePath ]

-- | Show IPFS object data. 
cat :: Text -> IO ()
cat hash = do 
    res <- call $ _cat hash
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> TextIO.putStr v

-- | Add a file or directory to ipfs. 
add :: Text -> IO()
add filePath = do 
    responseVal <- multipartCall (TextS.pack "http://localhost:5001/api/v0/add") filePath 
    print (decode (Net.responseBody responseVal)  :: Maybe AddObj)
    
-- | List directory contents for Unix filesystem objects. 
ls :: Text -> IO ()
ls hash = do 
    res <- call $ _ls hash
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v

-- | Download IPFS objects. 
get :: Text -> IO ()
get hash = do 
    res <- call $ _get hash
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v ->  do  Tar.unpack "getResponseDirectory" . Tar.read $ BS.fromStrict $ TextS.encodeUtf8 v
                        print "The content has been stored in getResponseDirectory."

-- | List links (references) from an object. 
refs :: Text -> IO ()
refs hash = streamCall $ _refs hash

-- | List all local references. 
refsLocal :: IO ()
refsLocal = streamCall _refsLocal
 
-- | List peers with open connections. 
swarmPeers :: IO ()
swarmPeers = do 
    res <- call _swarmPeers
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v        

-- | Open connection to a given address. 'peerId' has to be of the format - /ipfs/id 
swarmConnect :: Text -> IO ()
swarmConnect peerId = do 
    res <- call $ _swarmConnect (Just peerId)  
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v

-- | Close connection to a given address. 'peerId' has to be of the format - /ipfs/id 
swarmDisconnect :: Text -> IO ()
swarmDisconnect peerId = do 
    res <- call $ _swarmDisconnect (Just peerId)  
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v

-- | Manipulate address filters. 
swarmFilters :: IO ()
swarmFilters = do 
    res <- call _swarmFilters
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v

-- | Add an address filter. 'peerId' has to be of the format - /ip4/{IP addr of peer}/ipcidr/{ip network prefix} 
swarmFilterAdd :: Text -> IO ()
swarmFilterAdd filterParam = do 
    res <- call $ _swarmFilterAdd (Just filterParam)  
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v

-- | Remove an address filter. 
swarmFilterRm :: Text -> IO ()
swarmFilterRm filterParam = do 
    res <- call $ _swarmFilterRm (Just filterParam)  
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v  

-- | 'Show some diagnostic information on the bitswap agent. 
bitswapStat :: IO ()
bitswapStat = do 
    res <- call _bitswapStat
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v
        
-- | Show blocks currently on the wantlist. 
bitswapWL :: IO ()
bitswapWL = do 
    res <- call _bitswapWL
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v    
        
-- | Show the current ledger for a peer. 
bitswapLedger :: Text -> IO ()
bitswapLedger peerId = do 
    res <- call $ _bitswapLedger peerId
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v
        
-- | Trigger reprovider. 
bitswapReprovide :: IO ()
bitswapReprovide = do 
    res <- call $ _bitswapReprovide
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> TextIO.putStr v 

-- | List available multibase encodings. 
cidBases :: IO ()
cidBases = do 
    res <- call $ _cidBases
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v 
        
-- | List available CID codecs. 
cidCodecs :: IO ()
cidCodecs = do 
    res <- call $ _cidCodecs
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v 
        
-- | List available multihashes. 
cidHashes :: IO ()
cidHashes = do 
    res <- call $ _cidHashes
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v 

-- | Convert CIDs to Base32 CID version 1. 
cidBase32 :: Text -> IO ()
cidBase32 hash = do 
    res <- call $ _cidBase32 hash
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v
                
-- | Format and convert a CID in various useful ways. 
cidFormat :: Text-> IO ()
cidFormat hash = do 
    res <- call $ _cidFormat hash
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v  
        
-- | Get a raw IPFS block. 
blockGet :: Text -> IO ()
blockGet key = do 
    res <- call $ _blockGet key
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> TextIO.putStr v
        
-- | Store input as an IPFS block. 
blockPut :: Text -> IO()
blockPut filePath = do 
    responseVal <- multipartCall (TextS.pack "http://localhost:5001/api/v0/block/put") filePath 
    print (decode (Net.responseBody responseVal)  :: Maybe BlockObj)
        
-- | Print information of a raw IPFS block. 
blockStat :: Text -> IO ()
blockStat key = do 
    res <- call $ _blockStat key
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v

-- | Get a dag node from ipfs. 
dagGet :: Text -> IO ()
dagGet ref = do 
    res <- call $ _dagGet ref
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v ->  TextIO.putStr v 

-- | Resolve ipld block. 
dagResolve :: Text -> IO ()
dagResolve ref = do 
    res <- call $ _dagResolve ref
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v 

-- | Add a dag node to ipfs. 
dagPut :: Text -> IO()
dagPut filePath = do 
    responseVal <- multipartCall (TextS.pack "http://localhost:5001/api/v0/dag/put") filePath 
    print (decode (Net.responseBody responseVal)  :: Maybe DagPutObj)

-- | Get ipfs config values. 
configGet :: Text -> IO ()
configGet key = do 
    res <- call $ _configGet key
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v

-- | Set ipfs config values. 
configSet :: Text -> Text -> IO ()
configSet key value = do 
    res <- call $ _configSet key $ Just value
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v 

-- | Replace the config with the file at <filePath>. 
configReplace :: Text -> IO()
configReplace filePath = do 
    responseVal <- multipartCall (TextS.pack "http://localhost:5001/api/v0/config/replace") filePath 
    case statusCode $ Net.responseStatus responseVal of 
        200 -> putStrLn "Config File Replaced Successfully with status code - "
        _   -> putStrLn $ "Error occured with status code - "
    print $ statusCode $ Net.responseStatus responseVal
                
-- | Output the raw bytes of an IPFS object. 
objectData :: Text -> IO ()
objectData key = do 
    res <- call $ _objectData key
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> TextIO.putStr v

-- | Create a new object from an ipfs template. 
objectNew :: IO ()
objectNew = do 
    res <- call _objectNew
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v
 
-- | Output the links pointed to by the specified object. 
objectGetLinks :: Text -> IO ()
objectGetLinks key = do 
    res <- call $ _objectGetLinks key
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v

-- | Add a Merkle-link to the given object and return the hash of the result. 
objectAddLink ::  Text -> Text -> Text -> IO ()
objectAddLink hash name key = do 
    res <- call $ _objectAddLink hash (Just name) (Just key)
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v

-- | Remove a Merkle-link from the given object and return the hash of the result. 
objectRmLink :: Text -> Text -> IO ()
objectRmLink key name = do 
    res <- call $ _objectRmLink key (Just name)
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v

-- | Append data to what already exists in the data segment in the given object. 
objectAppendData :: Text -> Text -> IO()
objectAppendData key filePath = do 
    responseVal <- multipartCall ( ( TextS.pack "http://localhost:5001/api/v0/object/patch/append-data?arg=" ) <> key) filePath 
    print (decode ( Net.responseBody responseVal)  :: Maybe ObjectLinksObj)        

-- | Set the data field of an IPFS object. 
objectSetData :: Text -> Text -> IO()
objectSetData key filePath = do 
    responseVal <- multipartCall ( ( TextS.pack "http://localhost:5001/api/v0/object/patch/set-data?arg=" ) <>key) filePath 
    print (decode ( Net.responseBody responseVal)  :: Maybe ObjectLinksObj)        
        
-- | Get and serialize the DAG node named by key. 
objectGet :: Text -> IO ()
objectGet key = do 
    res <- call $ _objectGet key
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v 

-- | 'Display the diff between two ipfs objects. 
objectDiff :: Text -> Text -> IO ()
objectDiff firstKey secondKey = do 
    res <- call $ _objectDiff firstKey (Just secondKey)
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v 

-- | Store input as a DAG object, print its key. 
objectPut :: Text -> IO()
objectPut filePath = do 
    responseVal <- multipartCall (TextS.pack "http://localhost:5001/api/v0/object/put") filePath 
    print (decode ( Net.responseBody responseVal)  :: Maybe ObjectObj)        

-- | Get stats for the DAG node named by key. 
objectStat :: Text -> IO ()
objectStat key = do 
    res <- call $ _objectStat key
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v 

-- | Pin objects to local storage. 
pinAdd :: Text -> IO ()
pinAdd pinPath = do 
    res <- call $ _pinAdd pinPath
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v 

-- | Remove pinned objects from local storage. 
pinRemove :: Text -> IO ()
pinRemove pinPath = do 
    res <- call $ _pinRemove pinPath
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v

-- | Add peers to the bootstrap list. 
bootstrapAdd :: Text -> IO ()
bootstrapAdd peerId = do 
    res <- call $ _bootstrapAdd (Just peerId)
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v

-- | Show peers in the bootstrap list. 
bootstrapList :: IO ()
bootstrapList = do 
    res <- call $ _bootstrapList
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v

-- | Remove peers from the bootstrap list. 
bootstrapRM :: Text -> IO ()
bootstrapRM peerId = do 
    res <- call $ _bootstrapRM  (Just peerId)
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v

-- | Print ipfs bandwidth information. 
statsBw :: IO ()
statsBw = do 
    res <- call $ _statsBw  
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v

-- | Get stats for the currently used repo. 
statsRepo :: IO ()
statsRepo = do 
    res <- call $ _statsRepo  
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v

-- | Show ipfs version information. 
version :: IO ()
version = do 
    res <- call $ _version  
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v

-- | Show ipfs node id info. 
id :: IO ()
id = do 
    res <- call $ _id  
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v

-- | Show ipfs node id info of the given peerId. 
idPeer :: Text -> IO ()
idPeer peerId = do 
    res <- call $ _idPeer peerId  
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v

-- | Resolve DNS links. 
dns :: Text -> IO ()
dns name = do 
    res <- call $ _dns name  
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v

-- | Send echo request packets to IPFS hosts. 
ping :: Text -> IO ()
ping cid = streamCall $ _ping cid  

-- | Find the multiaddresses associated with the given peerId. 
dhtFindPeer :: Text -> IO ()
dhtFindPeer peerId = streamCall $ _dhtFindPeer peerId  

-- | Find peers that can provide a specific value, given a key. 
dhtFindProvs :: Text -> IO ()
dhtFindProvs cid = streamCall $ _dhtFindProvs cid  

-- | 'Given a key, query the routing system for its best value. 
dhtGet :: Text -> IO ()
dhtGet cid = streamCall $ _dhtGet cid  

-- | 'Announce to the network that you are providing given values. 
dhtProvide :: Text -> IO ()
dhtProvide cid = streamCall $ _dhtProvide cid 

-- | Find the closest Peer IDs to a given peerID by querying the DHT. 
dhtQuery ::  Text -> IO ()
dhtQuery peerId = streamCall $ _dhtQuery peerId

-- | List subscribed topics by name. 
pubsubLs :: IO ()
pubsubLs = do 
    res <- call _pubsubLs  
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v

-- | List peers we are currently pubsubbing with. 
pubsubPeers :: IO ()
pubsubPeers = do 
    res <- call _pubsubPeers
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v

-- | Publish a message to a given pubsub topic.
pubsubPublish :: Text -> Text -> IO ()
pubsubPublish topic mssg = do 
    res <- call $ _pubsubPublish topic $ Just mssg
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right _ -> putStrLn "The given message has been published."
     
-- | Subscribe to messages on a given topic.
pubsubSubscribe :: Text -> IO ()
pubsubSubscribe topic = streamCall $ _pubsubSubscribe topic

-- | List the logging subsystems. 
logLs :: IO ()
logLs = do 
    res <- call _logLs
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v

-- | Change the logging level. 
logLevel :: Text -> Text -> IO ()
logLevel subsystem level = do 
    res <- call $ _logLevel subsystem $ Just level
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v

-- | Read the event log. 
logTail :: IO ()
logTail = streamCall _logTail

-- | Show the repo version. 
repoVersion :: IO ()
repoVersion = do 
    res <- call _repoVersion
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v

-- | Remove repo lockfiles. 
repoFsck :: IO ()
repoFsck = do 
    res <- call _repoFsck
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v

-- | Perform a garbage collection sweep on the repo. 
repoGc :: IO ()
repoGc = streamCall _repoGc

-- | Verify all blocks in repo are not corrupted. 
repoVerify :: IO ()
repoVerify = streamCall _repoVerify

-- | 'List all local keypairs. 
keyList :: IO (Either ServantError KeyObj)
keyList = call _keyList

-- | Create a new keypair. 
keyGen :: Text -> Text -> IO (Either ServantError KeyDetailsObj) 
keyGen name keyType = call $ _keyGen name (Just keyType)

-- | Rename a keypair. 
keyRename :: Text -> Text -> IO (Either ServantError KeyRenameObj)
keyRename was now  = call $ _keyRename was $ Just now       

-- | Remove a keypair. 
keyRm :: Text -> IO (Either ServantError KeyObj)
keyRm name  = call $ _keyRm name 

-- | Change the cid version or hash function of the root node of a given mfsPath. 
filesChcidVer :: Text -> Int -> IO ()
filesChcidVer mfsPath cidVersion = do 
    res <- call $ _filesChcid (Just mfsPath) (Just cidVersion)
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right _ -> putStrLn "The directory's cid version has been changed."

-- | Copy files into mfs. 
filesCp :: Text -> Text -> IO ()
filesCp src dest  = do 
    res <- call $ _filesCp (Just src) (Just dest)
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right _ -> putStrLn "The object has been copied to the specified destination"

-- | Flush a given path's data to disk. 
filesFlush ::Text -> IO ()
filesFlush mfsPath = do 
    res <- call $ _filesFlush $ Just mfsPath 
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v

-- | List directories in the local mutable namespace. 
filesLs :: Text -> IO ()
filesLs mfsPath  = do 
    res <- call $ _filesLs $ Just mfsPath 
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v

-- | Make directories. 
filesMkdir :: Text -> IO ()
filesMkdir mfsPath  = do 
    res <- call $ _filesMkdir $ Just mfsPath 
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right _ -> putStrLn "The Directory has been created on the specified path."

-- | Move files. 
filesMv :: Text -> Text -> IO ()
filesMv src dest  = do 
    res <- call $ _filesMv (Just src) (Just dest)
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right _ -> putStrLn "The object has been moved to the specified destination"

-- | Read a file in a given mfs. 
filesRead :: Text -> IO ()
filesRead mfsPath  = do 
    res <- call $ _filesRead $ Just mfsPath 
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> TextIO.putStr v

-- | Display file status. 
filesStat :: Text -> IO ()
filesStat mfsPath  = do 
    res <- call $ _filesStat $ Just mfsPath 
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v

-- | Remove a file. 
filesRm :: Text -> IO ()
filesRm mfsPath  = do 
    res <- call $ _filesRm (Just mfsPath) (Just True)  
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right _ -> putStrLn "The object has been removed."

-- | Write to a mutable file in a given filesystem. 
filesWrite :: Text -> Text -> Bool -> IO()
filesWrite mfsPath filePath toTruncate = do 
    responseVal <- multipartCall ((TextS.pack "http://localhost:5001/api/v0/files/write?arg=") 
        <> mfsPath <> (TextS.pack "&create=true") <>  (TextS.pack "&truncate=") <> (TextS.pack $ show toTruncate) ) filePath 
    case statusCode $ Net.responseStatus responseVal of 
        200 -> putStrLn "Config File Replaced Successfully with status code - "
        _   -> putStrLn $ "Error occured with status code - "
    print $ statusCode $ Net.responseStatus responseVal    

-- | Shut down the ipfs daemon. 
shutdown :: IO ()
shutdown = do 
    res <- call $ _shutdown   
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right _ -> putStrLn "The daemon has been shutdown, your welcome."
