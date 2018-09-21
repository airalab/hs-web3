{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

-- |
-- Module      :  Network.Ethereum.Web3.Provider
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Web3 service provider.
--

module Network.Ethereum.Web3.Provider where

import           Control.Concurrent.Async      (Async, async)
import           Control.Exception             (Exception, try)
import           Control.Monad.Catch           (MonadThrow)
import           Control.Monad.IO.Class        (MonadIO (..))
import           Control.Monad.Reader          (MonadReader (..))
import           Control.Monad.Trans.Reader    (ReaderT, mapReaderT, runReaderT)
import           Data.Aeson                    (FromJSON (..), ToJSON (..),
                                                Value, object, withObject, (.:),
                                                (.:?), (.=))
import           Data.ByteString               (ByteString)
import           Data.Default                  (Default (..))
import           Data.Text                     (Text, pack, unpack)
import           GHC.Generics                  (Generic)
import           Network.Ethereum.Web3.Logging
import           Network.HTTP.Client           (Manager, newManager)

#ifdef TLS_MANAGER
import           Network.HTTP.Client.TLS       (tlsManagerSettings)
#else
import           Network.HTTP.Client           (defaultManagerSettings)
#endif

-- | Any communication with Ethereum node wrapped with 'Web3' monad
newtype Web3 a = Web3 { unWeb3 :: ReaderT (Provider, Web3Logger) IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow)

instance MonadReader (Provider, Web3Logger) Web3 where
    ask = Web3 ask
    local f = Web3 . local f . unWeb3

-- | Some peace of error response
data Web3Error
  = JsonRpcFail !String
  -- ^ JSON-RPC communication error
  | ParserFail  !String
  -- ^ Error in parser state
  | UserFail    !String
  -- ^ Common head for user errors
  deriving (Show, Eq, Generic)

instance Exception Web3Error

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
        \v -> RpcError <$> v .: pack "code"
                       <*> v .: pack "message"
                       <*> v .:? pack "data"

instance ToJSON RpcError where
    toJSON err = object $ [ pack "code"    .= errCode err
                          , pack "message" .= errMessage err
                          ] ++ maybeData
        where maybeData = case errData err of
                            Nothing -> []
                            Just d  -> [ pack "data" .= d ]

-- | JSON-RPC server URI
type ServerUri  = String

type TransformedJsonRpcRequest = Value
type ShortCircuitedJsonRpcResponse = Either RpcError Value

--TODO: Change to `HttpProvider ServerUri | IpcProvider FilePath` to support IPC
data JsonRpcProvider = HttpProvider       { getHttpManager :: IO Manager
                                          , serverUri      :: ServerUri
                                          }
                     | AbstractProvider   { runAbstractProvider :: Text -> [Value] -> IO ShortCircuitedJsonRpcResponse }
                     | HookedHTTPProvider { runHookedProvider   :: Text -> [Value] -> IO (Either TransformedJsonRpcRequest ShortCircuitedJsonRpcResponse)
                                          , getHttpManager      :: IO Manager
                                          , serverUri           :: ServerUri
                                          }
  deriving (Generic)

-- | Web3 Provider
data Provider = Provider { jsonRpc              :: JsonRpcProvider
                         , signingConfiguration :: Maybe SigningConfiguration
                         }

data SigningConfiguration = SigningConfiguration { privateKey      :: ByteString
                                                 , chainIdentifier :: Integer } deriving (Show, Eq)

instance Default Provider where
  def = Provider (HttpProvider getDefaultManager "http://localhost:8545") Nothing


-- This is down here to make stylish-haskell happy.
getDefaultManager :: IO Manager
#ifdef TLS_MANAGER
getDefaultManager = newManager tlsManagerSettings
#else
getDefaultManager = newManager defaultManagerSettings
#endif

runWeb3With' :: MonadIO m => Provider -> Web3Logger -> Web3 a -> m (Either Web3Error a)
runWeb3With' provider w3logger =
    liftIO . try .  flip runReaderT (provider, w3logger) . unWeb3

-- | 'Web3' monad runner, using the supplied Manager
runWeb3With :: MonadIO m => Provider -> Web3 a -> m (Either Web3Error a)
runWeb3With provider = runWeb3With' provider noopLogger

-- | 'Web3' monad runner
runWeb3' :: MonadIO m => Provider -> Web3 a -> m (Either Web3Error a)
runWeb3' provider f = runWeb3With provider f

-- | 'Web3' runner for default provider
runWeb3 :: MonadIO m => Web3 a -> m (Either Web3Error a)
{-# INLINE runWeb3 #-}
runWeb3 = runWeb3' def

-- | Fork 'Web3' with the same 'Provider'
forkWeb3 :: Web3 a -> Web3 (Async a)
{-# INLINE forkWeb3 #-}
forkWeb3 = Web3 . mapReaderT async . unWeb3

web3Trace :: String -> Web3 ()
web3Trace msg = do
    (_, logger) <- ask
    liftIO . unWeb3Logger logger $ W3LMTrace msg
