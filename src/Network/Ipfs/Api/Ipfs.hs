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

import           Data.Text                        as TextS
import           Network.HTTP.Client   (newManager, defaultManagerSettings)
import           Servant.Client

import           Network.Ipfs.Api.Api   (_cat, _ls, _refs, _refsLocal, 
                                        _swarmPeers, _bitswapStat, _bitswapWL,
                                        _bitswapLedger, _bitswapReprovide,
                                        _cidBases, _cidCodecs, _cidHashes, _cidBase32,
                                        _cidFormat, _blockGet, _blockStat, _dagGet,
                                        _dagResolve, _configGet, _configSet, _objectData,
                                        _objectNew, _objectGetLinks, _objectAddLink,
                                         _objectGet)

call :: ClientM a -> IO (Either ServantError a)
call func = do 
    manager' <- newManager defaultManagerSettings
    runClientM func (mkClientEnv manager' (BaseUrl Http "localhost" 5001 "/api/v0"))


cat :: Text -> IO ()
cat hash = do 
    res <- call $ _cat hash
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v
        
ls :: Text -> IO ()
ls hash = do 
    res <- call $ _ls hash
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v
        
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