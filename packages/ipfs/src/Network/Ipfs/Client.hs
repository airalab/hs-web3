{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      :  Network.Ipfs.Client
-- Copyright   :  Aleksandr Krupenkin 2016-2021
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unknown
--
-- IPFS daemon HTTP client.
--

module Network.Ipfs.Client where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Network.HTTP.Client  as Net hiding (Proxy)
import           Servant.Client

newtype IpfsT m a = IpfsT { unIpfs :: ReaderT (Manager, BaseUrl, String) (ExceptT ClientError m) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader (Manager, BaseUrl, String)
           , MonadError ClientError
           )

instance MonadTrans IpfsT where
  lift = IpfsT . lift . lift

type Ipfs a = IpfsT IO a

-- | 'IpfsT' monad runner.
runIpfs' :: BaseUrl -> Ipfs a -> IO ()
runIpfs' url ipfs = do
  manager' <- liftIO $ newManager defaultManagerSettings
  ret <- runExceptT (runReaderT (unIpfs ipfs) (manager', url, showBaseUrl url))
  case ret of
    Left err -> putStrLn $ "Error: " ++ show err
    Right _  -> putStr ""

-- | 'IpfsT' monad runner with default arguments.
runIpfs :: Ipfs a -> IO ()
runIpfs = runIpfs' (BaseUrl Http "localhost" 5001 "/api/v0")
