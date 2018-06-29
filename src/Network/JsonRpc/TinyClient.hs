{-# LANGUAGE CPP                    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

-- |
-- Module      :  Network.JsonRpc.TinyClient
-- Copyright   :  Alexander Krupenkin 2016-2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Tiny JSON-RPC 2.0 client.
-- Functions for implementing the client side of JSON-RPC 2.0.
-- See <http://www.jsonrpc.org/specification>.
--
-- If you have monad with 'MonadIO', 'MonadThrow' and 'MonadReader' instances,
-- it can be used as base for JSON-RPC calls.
--
-- Example:
--
-- @
--   newtype MyMonad a = ...
--
--   foo :: Int -> Bool -> Mymonad Text
--   foo = remote "foo"
-- @
--
-- Arguments of function are stored into @params@ request array.
--
-- Example:
--
-- @
--   myMethod :: JsonRpcM m => Int -> Bool -> m String
--   myMethod = remote "myMethod"
-- @
--

module Network.JsonRpc.TinyClient (
    JsonRpcException(..)
  , defaultSettings
  , JsonRpcClient
  , jsonRpcServer
  , jsonRpcManager
  , RpcError(..)
  , MethodName
  , JsonRpcM
  , remote
  ) where

import           Control.Applicative     ((<|>))
import           Control.Exception       (Exception)
import           Control.Monad           ((<=<))
import           Control.Monad.Catch     (MonadThrow, throwM)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Monad.State     (MonadState)
import           Data.Aeson              (FromJSON (..), ToJSON (..),
                                          Value (String), eitherDecode, encode,
                                          object, withObject, (.:), (.:?), (.=))
import           Data.ByteString.Lazy    (ByteString)
import           Data.Text               (Text, unpack)
import           Lens.Micro.Mtl          (use)
import           Lens.Micro.TH           (makeLenses)
import           Network.HTTP.Client     (Manager, RequestBody (RequestBodyLBS),
                                          httpLbs, method, newManager,
                                          parseRequest, requestBody,
                                          requestHeaders, responseBody)

#ifdef TLS_MANAGER
import           Network.HTTP.Client.TLS (tlsManagerSettings)
#else
import           Network.HTTP.Client     (defaultManagerSettings)
#endif

-- | Name of called method.
type MethodName = Text

-- | Remote call monad constrait
type JsonRpcM m = (MonadIO m, MonadThrow m, MonadState JsonRpcClient m)

-- | JSON-RPC client state vars
data JsonRpcClient = JsonRpcClient
  { _jsonRpcManager :: Manager
  -- ^ HTTP connection manager
  , _jsonRpcServer  :: String
  -- ^ Remote server URI
  }

$(makeLenses ''JsonRpcClient)

defaultSettings :: MonadIO m
                => String
                -- ^ JSON-RPC server URI
                -> m JsonRpcClient
defaultSettings srv = liftIO $ JsonRpcClient
#ifdef TLS_MANAGER
  <$> newManager tlsManagerSettings
#else
  <$> newManager defaultManagerSettings
#endif
  <*> pure srv

instance Show JsonRpcClient where
    show JsonRpcClient{..} = "JsonRpcClient<" ++ _jsonRpcServer ++ ">"

-- | JSON-RPC request.
data Request = Request { rqMethod :: !Text
                       , rqId     :: !Int
                       , rqParams :: !Value }

instance ToJSON Request where
    toJSON rq = object [ "jsonrpc" .= String "2.0"
                       , "method"  .= rqMethod rq
                       , "params"  .= rqParams rq
                       , "id"      .= rqId rq ]

-- | JSON-RPC response.
data Response = Response
  { rsResult :: !(Either RpcError Value)
  } deriving (Eq, Show)

instance FromJSON Response where
    parseJSON =
        withObject "JSON-RPC response object" $
            \v -> Response <$>
                (Right <$> v .: "result" <|> Left <$> v .: "error")

-- | JSON-RPC error message
data RpcError = RpcError
  { errCode    :: !Int
  , errMessage :: !Text
  , errData    :: !(Maybe Value)
  } deriving Eq

instance Show RpcError where
    show (RpcError code msg dat) =
        "JSON-RPC error " ++ show code ++ ": " ++ unpack msg
         ++ ". Data: " ++ show dat

instance FromJSON RpcError where
    parseJSON = withObject "JSON-RPC error object" $
        \v -> RpcError <$> v .: "code"
                       <*> v .: "message"
                       <*> v .:? "data"

data JsonRpcException
    = ParsingException String
    | CallException RpcError
    deriving (Eq, Show)

instance Exception JsonRpcException

class JsonRpcM m => Remote m a | a -> m where
    remote' :: ([Value] -> m ByteString) -> a

instance (ToJSON a, Remote m b) => Remote m (a -> b) where
    remote' f x = remote' (\xs -> f (toJSON x : xs))

instance {-# INCOHERENT #-} (JsonRpcM m, FromJSON b) => Remote m (m b) where
    remote' f = decodeResponse =<< f []

-- | Remote call of JSON-RPC method.
remote :: Remote m a => MethodName -> a
remote = remote' . call

call :: JsonRpcM m
     => MethodName
     -> [Value]
     -> m ByteString
call n = connection . encode . Request n 1 . toJSON
  where
    connection body = do
        serverUri <- use jsonRpcServer
        request <- parseRequest serverUri
        let request' = request
                     { requestBody = RequestBodyLBS body
                     , requestHeaders = [("Content-Type", "application/json")]
                     , method = "POST" }
        manager <- use jsonRpcManager
        responseBody <$> liftIO (httpLbs request' manager)

decodeResponse :: (MonadThrow m, FromJSON a)
               => ByteString
               -> m a
decodeResponse = (tryParse . eitherDecode . encode)
               <=< tryResult . rsResult
               <=< tryParse . eitherDecode
  where
    tryParse = either (throwM . ParsingException) return
    tryResult = either (throwM . CallException) return
