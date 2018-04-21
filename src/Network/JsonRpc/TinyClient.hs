{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
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

module Network.JsonRpc.TinyClient (
    JsonRpcException(..)
  , RpcError(..)
  , MethodName
  , ServerUri
  , Remote
  , remote
  ) where

import           Control.Applicative    ((<|>))
import           Control.Exception      (Exception)
import           Control.Monad          ((<=<))
import           Control.Monad.Catch    (MonadThrow, throwM)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, ask)
import           Data.Aeson
import           Data.ByteString.Lazy   (ByteString)
import           Data.Text              (Text, unpack)
import           Network.HTTP.Client    (Manager, RequestBody (RequestBodyLBS),
                                         httpLbs, method, parseRequest,
                                         requestBody, requestHeaders,
                                         responseBody)

-- | Name of called method.
type MethodName = Text

-- | JSON-RPC server URI
type ServerUri  = String

-- | JSON-RPC minimal client config
type Config = (ServerUri, Manager)

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
                       <*> v .: "data"

-- | Typeclass for JSON-RPC monad base.
--
-- If you have monad with 'MonadIO', 'MonadThrow' and 'MonadReader' instances,
-- it can be used as base for JSON-RPC calls.
--
-- Example:
--
-- @
--   newtype MyMonad a = ...
--
--   instance Remote MyMonad (Mymonad a)
--
--   foo :: Int -> Bool -> Mymonad Text
--   foo = remote "foo"
-- @
--
class (MonadIO m, MonadThrow m, MonadReader Config m) => Remote m a | a -> m where
    remote_ :: ([Value] -> m ByteString) -> a

    default remote_ :: (FromJSON b, m b ~ a) => ([Value] -> m ByteString) -> a
    remote_ f = decodeResponse =<< f []

instance (ToJSON a, Remote m b) => Remote m (a -> b) where
    remote_ f x = remote_ (\xs -> f (toJSON x : xs))

-- | Remote call of JSON-RPC method.
--
-- Arguments of function are stored into @params@ request array.
--
-- Example:
--
-- @
--   myMethod :: Int -> Bool -> m String
--   myMethod = remote "myMethod"
-- @
--
remote :: Remote m a => MethodName -> a
{-# INLINE remote #-}
remote = remote_ . call

call :: (MonadIO m,
         MonadThrow m,
         MonadReader Config m)
     => MethodName
     -> [Value]
     -> m ByteString
call n = connection . encode . Request n 1 . toJSON
  where
    connection body = do
        (uri, manager) <- ask
        request <- parseRequest uri
        let request' = request
                     { requestBody = RequestBodyLBS body
                     , requestHeaders = [("Content-Type", "application/json")]
                     , method = "POST" }
        responseBody <$> liftIO (httpLbs request' manager)

data JsonRpcException
    = ParsingException String
    | CallException RpcError
    deriving (Eq, Show)

instance Exception JsonRpcException

decodeResponse :: (MonadThrow m, FromJSON a)
               => ByteString
               -> m a
decodeResponse = (tryParse . eitherDecode . encode)
               <=< tryResult . rsResult
               <=< tryParse . eitherDecode
  where
    tryParse = either (throwM . ParsingException) return
    tryResult = either (throwM . CallException) return
