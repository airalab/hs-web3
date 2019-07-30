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

import           Network.HTTP.Client   (newManager, defaultManagerSettings)
import           Servant.Client

import           Network.Ipfs.Api.Api   (_cat, _ls, _refs, _refsLocal, 
                                        _swarmPeers, _bitswapStat, _bitswapWL,
                                        _bitswapLedger, _bitswapReprovide,
                                        _cidBases, _cidCodecs, _cidHashes, _cidBase32)

call :: ClientM a -> IO (Either ServantError a)
call func = do 
    manager' <- newManager defaultManagerSettings
    runClientM func (mkClientEnv manager' (BaseUrl Http "localhost" 5001 "/api/v0"))


cat :: String -> IO ()
cat hash = do 
    res <- call $ _cat hash
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v
        
ls :: String -> IO ()
ls hash = do 
    res <- call $ _ls hash
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v
        
refs :: String -> IO ()
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
        
bitswapLedger :: String -> IO ()
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

cidBase32 :: String -> IO ()
cidBase32 hash = do 
    res <- call $ _cidBase32 hash
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right v -> print v       