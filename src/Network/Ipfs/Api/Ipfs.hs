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

import qualified Codec.Archive.Tar            as Tar
import           Data.Aeson                   (decode)
import           Data.Text                    as TextS
import qualified Data.Text.Encoding           as TextS
import qualified Data.ByteString.Lazy         as BS (ByteString, fromStrict) 
import           Network.HTTP.Client          as Net  hiding (Proxy)
import           Network.HTTP.Client.MultipartFormData
import           Servant.Client

import           Network.Ipfs.Api.Api         (_cat, _ls, _get, _refs, _refsLocal, _swarmPeers, _swarmConnect,
                                              _swarmDisconnect, _swarmFilterAdd, _swarmFilters,
                                              _swarmFilterRm, _bitswapStat, _bitswapWL, _bitswapLedger,
                                              _bitswapReprovide, _cidBases, _cidCodecs, _cidHashes, _cidBase32,
                                              _cidFormat, _blockGet, _blockStat, _dagGet,
                                              _dagResolve, _configGet, _configSet, _objectData,
                                              _objectNew, _objectGetLinks, _objectAddLink,
                                              _objectGet, _objectStat, _pinAdd, _pinRemove,_bootstrapList, 
                                              _bootstrapAdd, _bootstrapRM, _statsBw, _statsRepo, _version,
                                              _id, _idPeer, _dns, _shutdown, BlockObj)

import           Network.Ipfs.Api.Multipart   (AddObj)

call :: ClientM a -> IO (Either ServantError a)
call func = do 
    manager' <- newManager defaultManagerSettings
    runClientM func (mkClientEnv manager' (BaseUrl Http "localhost" 5001 "/api/v0"))

multipartCall ::  Text -> Text -> IO BS.ByteString
multipartCall uri filePath = do
    reqManager <- newManager defaultManagerSettings
    req <- parseRequest $ TextS.unpack uri
    resp <- flip httpLbs reqManager =<< formDataBody form req
    return (Net.responseBody resp)
    
    where form = [ partFileSource "file" $ TextS.unpack filePath ]



cat :: Text -> IO ()
cat hash = do 
    res <- call $ _cat hash
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v

add :: Text -> IO()
add filePath = do 
    respBody <- multipartCall (TextS.pack "http://localhost:5001/api/v0/add") filePath 
    print (decode (respBody)  :: Maybe AddObj)
    
ls :: Text -> IO ()
ls hash = do 
    res <- call $ _ls hash
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v

get :: Text -> IO ()
get hash = do 
    res <- call $ _get hash
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v ->  Tar.unpack "getResponseDirectory" . Tar.read $ BS.fromStrict $ TextS.encodeUtf8 v

refs :: Text -> IO ()
refs hash = do 
    res <- call $ _refs hash
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v  

refsLocal :: IO ()
refsLocal = do 
    res <- call _refsLocal
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v
 
swarmPeers :: IO ()
swarmPeers = do 
    res <- call _swarmPeers
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v        

-- | peerId has to be of the format - /ipfs/id        
swarmConnect :: Text -> IO ()
swarmConnect peerId = do 
    res <- call $ _swarmConnect (Just peerId)  
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v

-- | peerId has to be of the format - /ipfs/id        
swarmDisconnect :: Text -> IO ()
swarmDisconnect peerId = do 
    res <- call $ _swarmDisconnect (Just peerId)  
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v

swarmFilters :: IO ()
swarmFilters = do 
    res <- call _swarmFilters
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v

-- | peerId has to be of the format - /ip4/{IP addr of peer}/ipcidr/{ip network prefix}       
swarmFilterAdd :: Text -> IO ()
swarmFilterAdd filterParam = do 
    res <- call $ _swarmFilterAdd (Just filterParam)  
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v

swarmFilterRm :: Text -> IO ()
swarmFilterRm filterParam = do 
    res <- call $ _swarmFilterRm (Just filterParam)  
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v  

bitswapStat :: IO ()
bitswapStat = do 
    res <- call _bitswapStat
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v
        
bitswapWL :: IO ()
bitswapWL = do 
    res <- call _bitswapWL
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v    
        
bitswapLedger :: Text -> IO ()
bitswapLedger peerId = do 
    res <- call $ _bitswapLedger peerId
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v
        
bitswapReprovide :: IO ()
bitswapReprovide = do 
    res <- call $ _bitswapReprovide
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v 

cidBases :: IO ()
cidBases = do 
    res <- call $ _cidBases
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v 
        
cidCodecs :: IO ()
cidCodecs = do 
    res <- call $ _cidCodecs
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v 
        
cidHashes :: IO ()
cidHashes = do 
    res <- call $ _cidHashes
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v 

cidBase32 :: Text -> IO ()
cidBase32 hash = do 
    res <- call $ _cidBase32 hash
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v
                
cidFormat :: Text-> IO ()
cidFormat hash = do 
    res <- call $ _cidFormat hash
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v  
        
blockGet :: Text -> IO ()
blockGet key = do 
    res <- call $ _blockGet key
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v
        
blockPut :: Text -> IO()
blockPut filePath = do 
    respBody <- multipartCall (TextS.pack "http://localhost:5001/api/v0/block/put") filePath 
    print (decode (respBody)  :: Maybe BlockObj)
        
blockStat :: Text -> IO ()
blockStat key = do 
    res <- call $ _blockStat key
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v

dagGet :: Text -> IO ()
dagGet ref = do 
    res <- call $ _dagGet ref
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v 

dagResolve :: Text -> IO ()
dagResolve ref = do 
    res <- call $ _dagResolve ref
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v 

configGet :: Text -> IO ()
configGet key = do 
    res <- call $ _configGet key
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v 

configSet :: Text -> Text -> IO ()
configSet key value = do 
    res <- call $ _configSet key $ Just value
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v 

objectData :: Text -> IO ()
objectData key = do 
    res <- call $ _objectData key
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v

objectNew :: IO ()
objectNew = do 
    res <- call _objectNew
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v
 
objectGetLinks :: Text -> IO ()
objectGetLinks key = do 
    res <- call $ _objectGetLinks key
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v

objectAddLink ::  Text -> Text -> Text -> IO ()
objectAddLink hash name key = do 
    res <- call $ _objectAddLink hash (Just name) (Just key)
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v
                
objectGet :: Text -> IO ()
objectGet key = do 
    res <- call $ _objectGet key
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v 

objectStat :: Text -> IO ()
objectStat key = do 
    res <- call $ _objectStat key
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v 

pinAdd :: Text -> IO ()
pinAdd pinPath = do 
    res <- call $ _pinAdd pinPath
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v 

pinRemove :: Text -> IO ()
pinRemove pinPath = do 
    res <- call $ _pinRemove pinPath
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v

bootstrapAdd :: Text -> IO ()
bootstrapAdd peerId = do 
    res <- call $ _bootstrapAdd (Just peerId)
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v

bootstrapList :: IO ()
bootstrapList = do 
    res <- call $ _bootstrapList
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v

bootstrapRM :: Text -> IO ()
bootstrapRM peerId = do 
    res <- call $ _bootstrapRM  (Just peerId)
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v

statsBw :: IO ()
statsBw = do 
    res <- call $ _statsBw  
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v

statsRepo :: IO ()
statsRepo = do 
    res <- call $ _statsRepo  
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v

version :: IO ()
version = do 
    res <- call $ _version  
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v

id :: IO ()
id = do 
    res <- call $ _id  
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v

idPeer :: Text -> IO ()
idPeer peerId = do 
    res <- call $ _idPeer peerId  
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v

dns :: Text -> IO ()
dns name = do 
    res <- call $ _dns name  
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v

shutdown :: IO ()
shutdown = do 
    res <- call $ _shutdown   
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right _ -> print "The daemon has been shutdown, your welcome."
