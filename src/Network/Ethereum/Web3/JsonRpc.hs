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
module Network.Ethereum.Web3.JsonRpc (remote, MethodName) where

import Network.Ethereum.Web3.Types

import Network.HTTP.Client (httpLbs, newManager, defaultManagerSettings,
                            requestBody, responseBody, method,
                            requestHeaders, parseRequest,
                            RequestBody(RequestBodyLBS))
import Control.Monad.Error.Class (throwError)
import Data.ByteString.Lazy (ByteString)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.IO.Class (liftIO)
import Control.Applicative ((<|>))
import Control.Monad.Trans (lift)
import Data.Vector (fromList)
import Control.Monad ((>=>))
import Data.Text (Text)
import Data.Aeson

-- | Name of called method.
type MethodName = Text

-- | Remote call of JSON-RPC method.
-- Arguments of function are stored into @params@ request array.
remote :: Remote a => MethodName -> a
remote n = remote_ (call . Array . fromList)
  where connection body = do
            conf <- ask
            liftIO $ do
                manager <- newManager defaultManagerSettings
                request <- parseRequest (rpcUri conf)
                let request' = request
                             { requestBody = RequestBodyLBS body
                             , requestHeaders = [("Content-Type", "application/json")]
                             , method = "POST" }
                responseBody <$> httpLbs request' manager
        call = connection . encode . Request n 1

class Remote a where
    remote_ :: ([Value] -> Web3 ByteString) -> a

instance (ToJSON a, Remote b) => Remote (a -> b) where
    remote_ f x = remote_ (\xs -> f (toJSON x : xs))

decodeResponse :: FromJSON a => ByteString -> Web3 a
decodeResponse = tryParse . eitherDecode
             >=> tryJsonRpc . rsResult
             >=> tryParse . eitherDecode . encode
  where tryJsonRpc :: Either RpcError a -> Web3 a
        tryJsonRpc (Right a) = return a
        tryJsonRpc (Left e)  = lift $ throwError (JsonRpcFail e)
        tryParse :: Either String a -> Web3 a
        tryParse   (Right a) = return a
        tryParse   (Left e)  = lift $ throwError (ParserFail e)

instance FromJSON a => Remote (Web3 a) where
    remote_ f = decodeResponse =<< f []

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
