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

import           Control.Arrow                    (left)
import           Data.ByteString.Lazy             (fromStrict, toStrict)
import           Data.Proxy           
import           Data.Typeable            
import           Network.HTTP.Client              (newManager, defaultManagerSettings)
import           Servant.API
import           Servant.Client
import qualified Data.ByteString.Lazy.Char8       as BC
import qualified Data.Text                        as TextS
import qualified Data.Text.Encoding               as TextS
import qualified Network.HTTP.Media               as M ((//), (/:))
import           Network.Ipfs.Api.Api             (IpfsReturnType, _cat)

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