{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
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

import           Control.Applicative            ((<|>))
import           Control.Exception              (Exception)
import           Control.Monad                  ((<=<))
import           Control.Monad.Catch            (MonadThrow, throwM)
import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Control.Monad.Reader           (MonadReader, ask)
import           Data.Aeson
import           Data.ByteString.Lazy           (ByteString)
import           Data.Text                      (Text)
import           Network.Ethereum.Web3.Logging
import           Network.Ethereum.Web3.Provider
import           Network.HTTP.Client            (RequestBody (RequestBodyLBS),
                                                 httpLbs, method, parseRequest,
                                                 requestBody, requestHeaders,
                                                 responseBody)
import           System.Random                  (randomIO)

instance FromJSON a => Remote Web3 (Web3 a)

-- | Name of called method.
type MethodName = Text

-- | JSON-RPC minimal client config
type Config = (Provider, Web3Logger)

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

instance ToJSON Response where
    toJSON r = case rsResult r of
        Left err -> object [ "error" .= err ]
        Right ok -> object [ "result" .= ok ]

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
call m r = do
  rid <- abs <$> liftIO randomIO
  let req = Request m rid (toJSON r)
      req' = toJSON req
  (Provider p _, logger) <- ask
  liftIO . unWeb3Logger logger $ W3LMJsonRPCRequest rid req'
  resp <- runProvider p req
  acceptAndLogResponse rid logger resp
  where
    acceptAndLogResponse rid logger resp = do
        liftIO . unWeb3Logger logger $ W3LMJsonRPCRawResponse rid resp
        return resp

    runProvider HttpProvider{..} req = connection (encode req) getHttpManager serverUri

    runProvider HookedHTTPProvider{..} _ = do
        ranHook <- liftIO $ runHookedProvider m r
        case ranHook of
            Left xformReq -> connection (encode xformReq) getHttpManager serverUri
            Right shortedResponse -> return $ encode (Response shortedResponse)

    runProvider AbstractProvider{..} _ = liftIO $ encode . Response <$> runAbstractProvider m r

    connection body getManager uri = do
        request <- parseRequest uri
        let request' = request
                     { requestBody = RequestBodyLBS body
                     , requestHeaders = [("Content-Type", "application/json")]
                     , method = "POST"
                     }
        manager <- liftIO getManager
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
