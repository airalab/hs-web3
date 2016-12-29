{-# LANGUAGE FlexibleInstances #-}
-- |
-- Module      :  Network.JsonRpc
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Little JSON-RPC 2.0 client.
-- Functions for implementing the client side of JSON-RPC 2.0.
-- See <http://www.jsonrpc.org/specification>.
--
module Network.Ethereum.Web3.JsonRpc (
    remote
  , MethodName
  , ServerUri
  ) where

import Network.Ethereum.Web3.Types

import Network.HTTP.Client (httpLbs, newManager, requestBody,
                            responseBody, method, requestHeaders,
                            parseRequest, RequestBody(RequestBodyLBS))
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.ByteString.Lazy (ByteString)
import Control.Applicative ((<|>))
import Control.Exception (throwIO)
import Data.Vector (fromList)
import Control.Monad ((>=>))
import Data.Text (Text)
import Data.Aeson

-- | Name of called method.
type MethodName = Text

-- | JSON-RPC server URI
type ServerUri  = String

-- | Remote call of JSON-RPC method.
-- Arguments of function are stored into @params@ request array.
remote :: Remote a => MethodName -> a
remote n = remote_ (\uri -> call uri . Array . fromList)
  where
    call uri = connection uri . encode . Request n 1
    connection uri body = do
        manager <- newManager tlsManagerSettings
        request <- parseRequest uri
        let request' = request
                     { requestBody = RequestBodyLBS body
                     , requestHeaders = [("Content-Type", "application/json")]
                     , method = "POST" }
        responseBody <$> httpLbs request' manager

decodeResponse :: FromJSON a => ByteString -> IO a
decodeResponse = tryParse . eitherDecode
             >=> tryJsonRpc . rsResult
             >=> tryParse . eitherDecode . encode
  where tryJsonRpc :: Either RpcError a -> IO a
        tryJsonRpc = either (throwIO . JsonRpcFail) return
        tryParse :: Either String a -> IO a
        tryParse = either (throwIO . ParserFail) return

class Remote a where
    remote_ :: (ServerUri -> [Value] -> IO ByteString) -> a

instance (ToJSON a, Remote b) => Remote (a -> b) where
    remote_ f x = remote_ (\u xs -> f u (toJSON x : xs))

instance (Provider p, FromJSON a) => Remote (Web3 p a) where
    remote_ f = (\u -> Web3 (decodeResponse =<< f u [])) =<< rpcUri

-- | JSON-RPC request.
data Request = Request { rqMethod :: Text
                       , rqId     :: Int
                       , rqParams :: Value }

instance ToJSON Request where
    toJSON rq = object $ [ "jsonrpc" .= String "2.0"
                         , "method"  .= rqMethod rq
                         , "params"  .= rqParams rq
                         , "id"      .= rqId rq ]

-- | JSON-RPC response.
data Response = Response
  { rsResult :: Either RpcError Value
  } deriving (Show)

instance FromJSON Response where
    parseJSON = withObject "JSON-RPC response object" $
                \v -> Response <$>
                      (Right <$> v .: "result" <|> Left <$> v .: "error")
