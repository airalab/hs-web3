{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

-- |
-- Module      :  Network.Ipfs.Api.Internal.Call
-- Copyright   :  Aleksandr Krupenkin 2016-2020
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unknown
--
-- Module containing IPFS API call functions.
--

module Network.Ipfs.Api.Internal.Call where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.ByteString.Lazy                  (ByteString)
import           Data.Text                             (Text, pack, unpack)
import           Network.HTTP.Client                   as Net hiding (Proxy)
import           Network.HTTP.Client.MultipartFormData
import           Servant.Client
import qualified Servant.Client.Streaming              as S
import           Servant.Types.SourceT                 (SourceT (..), foreach)

import           Network.Ipfs.Client                   (IpfsT)

-- | Regular Call function.
call :: MonadIO m => ClientM a -> IpfsT m a
call func = do
  (manager', url, _) <- ask
  resp <- liftIO (runClientM func (mkClientEnv manager' url))
  case resp of
    Left l  -> throwError l
    Right r -> return r

-- | Call function for ‘multipart/form-data’.
multipartCall :: MonadIO m => Text -> Text -> IpfsT m (Net.Response ByteString)
multipartCall funcUri filePath = do
    (reqManager, _, url) <- ask
    req <- liftIO $ parseRequest $ unpack (pack url <> "/" <> funcUri )
    liftIO $ flip httpLbs reqManager =<< formDataBody form req
  where
    form = [ partFileSource "file" $ unpack filePath ]

-- | Call function for Streams.
streamCall :: (MonadIO m, Show a) => S.ClientM (SourceT IO a) -> m ()
streamCall func = liftIO $ do
    manager' <- newManager defaultManagerSettings
    S.withClientM func (S.mkClientEnv manager' (BaseUrl Http "localhost" 5001 "/api/v0")) $ \e -> case e of
        Left err -> putStrLn $ "Error: " ++ show err
        Right rs -> foreach fail print rs
