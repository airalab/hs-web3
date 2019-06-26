{-# LANGUAGE CPP                    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
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
--   instance JsonRpc MyMonad
--
--   foo :: Mymonad Text
--   foo = remote "foo"
-- @
--
-- Arguments of function are stored into @params@ request array.
--
-- Example:
--
-- @
--   myMethod :: JsonRpc m => Int -> Bool -> m String
--   myMethod = remote "myMethod"
-- @
--

module Network.JsonRpc.TinyClient
    (
    -- * The JSON-RPC remote call monad
      JsonRpc(..)
    , MethodName

    -- * JSON-RPC client settings
    , JsonRpcClient
    , defaultSettings
    , jsonRpcServer
    , jsonRpcManager

    -- * Error handling
    , JsonRpcException(..)
    , RpcError(..)
    ) where

import           Control.Applicative     ((<|>))
import           Control.Exception       (Exception)
import           Control.Monad           ((<=<),forever, unless)
import           Control.Monad.Catch     (MonadThrow (..))
import           Control.Monad.IO.Class  (MonadIO (..))
import           Control.Monad.State     (MonadState,get)
import           Crypto.Number.Generate  (generateMax)
import           Data.Aeson              (FromJSON (..), ToJSON (..),
                                          Value (String), eitherDecode, encode,
                                          object, withObject, (.:), (.:?), (.=))
import           Data.ByteString.Lazy    (ByteString)
import           Data.Text               (Text, unpack)
import           Network.HTTP.Client     (Manager, RequestBody (RequestBodyLBS),
                                          httpLbs, method, newManager,
                                          parseRequest, requestBody,
                                          requestHeaders, responseBody)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.WebSockets      (Connection)

-- | JSON-RPC monad constrait.
type JsonRpcM m = (MonadIO m, MonadThrow m, MonadState JsonRpcClient m)

-- | JSON-RPC client state vars.
data JsonRpcClient = JsonRpcHTTPClient
    { jsonRpcManager :: Manager    -- ^ HTTP connection manager.
    , jsonRpcServer  :: String     -- ^ Remote server URI.
    }              | JsonRpcWSClient
    { jsonRpcWSConn :: Connection    -- ^ WS connection.
    , jsonRpcWSServer  :: String     -- ^ Remote server URI.
    }                 

-- $(makeLensesFor [("_jsonRpcManager", "jsonRpcManager"), ("_jsonRpcServer", "jsonRpcServer")] ''JsonRpcClient)

-- | Create default 'JsonRpcClient' settings.
defaultSettings :: MonadIO m
                => String           -- ^ JSON-RPC server URI
                -> m JsonRpcClient
defaultSettings srv = liftIO $ JsonRpcHTTPClient
  <$> newManager tlsManagerSettings
  <*> pure srv

instance Show JsonRpcClient where
    show JsonRpcHTTPClient{..} = "JsonRpcClient<" ++ jsonRpcServer ++ ">"
    show JsonRpcWSClient{..} = "JsonRpcClient<" ++ jsonRpcWSServer ++ ">"

-- | JSON-RPC request.
data Request = Request
    { rqMethod :: !Text
    , rqId     :: !Int
    , rqParams :: !Value
    } deriving (Eq, Show)

instance ToJSON Request where
    toJSON rq = object [ "jsonrpc" .= String "2.0"
                       , "method"  .= rqMethod rq
                       , "params"  .= rqParams rq
                       , "id"      .= rqId rq
                       ]

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
    deriving (Show, Eq)

instance Exception JsonRpcException

class JsonRpcM m => Remote m a | a -> m where
    remote' :: ([Value] -> m ByteString) -> a

instance (ToJSON a, Remote m b) => Remote m (a -> b) where
    remote' f x = remote' (\xs -> f (toJSON x : xs))

instance {-# INCOHERENT #-} (JsonRpcM m, FromJSON b) => Remote m (m b) where
    remote' f = decodeResponse =<< f []

-- | Name of called method.
type MethodName = Text

-- | JSON-RPC call monad.
class JsonRpcM m => JsonRpc m where
    -- | Remote call of JSON-RPC method.
    remote :: Remote m a => MethodName -> a
    {-# INLINE remote #-}
    remote = remote' . call

call :: JsonRpcM m
     => MethodName
     -> [Value]
     -> m ByteString
call m r = do
  rid <- liftIO $ generateMax maxInt
  connection . encode $ Request m (fromInteger rid) (toJSON r)
  where
    maxInt = toInteger (maxBound :: Int)
    connection body = do
        jsonRpcInstance <- get
        case jsonRpcInstance of { JsonRpcHTTPClient{..} -> do
            request <- parseRequest jsonRpcServer
            let request' = request
                         { requestBody = RequestBodyLBS body
                         , requestHeaders = [("Content-Type", "application/json")]
                         , method = "POST"
                         }
            responseBody <$> liftIO (httpLbs request' jsonRpcManager) }


decodeResponse :: (MonadThrow m, FromJSON a)
               => ByteString
               -> m a
decodeResponse = (tryParse . eitherDecode . encode)
               <=< tryResult . rsResult
               <=< tryParse . eitherDecode
  where
    tryParse = either (throwM . ParsingException) return
    tryResult = either (throwM . CallException) return
