{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module      :  Network.Ethereum.Web3.Types
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  MIT
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Common used types and instances.
--
module Network.Ethereum.Web3.Types where

import Network.Ethereum.Web3.Internal (toLowerFirst)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Network.Ethereum.Web3.Address (Address)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Default.Class (Default(..))
import qualified Data.Text.Lazy.Builder.Int as B
import qualified Data.Text.Lazy.Builder     as B
import qualified Data.Text.Read as R
import Data.Default.Class (def)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Aeson.TH
import Data.Aeson

-- | Main monad type
type Web3 = ReaderT Config (ExceptT Error IO)

-- | Web3 configuration
data Config = Config
  { rpcUri :: String
  -- ^ JSON-RPC node URI
  } deriving (Show, Eq)

instance Default Config where
    def = Config "http://localhost:8545"

data Error = JsonRpcFail RpcError
           | ParserFail  String
           | UserFail    String
  deriving (Show, Eq)

-- | Run 'Web3' monad with default config.
runWeb3 :: MonadIO m => Web3 a -> m (Either Error a)
runWeb3 = runWeb3' def

-- | Run 'Web3' monad.
runWeb3' :: MonadIO m => Config -> Web3 a -> m (Either Error a)
runWeb3' c = liftIO . runExceptT . flip runReaderT c

-- | JSON-RPC error.
data RpcError = RpcError
  { errCode     :: Int
  , errMessage  :: Text
  , errData     :: Maybe Value
  } deriving (Show, Eq)

$(deriveJSON (defaultOptions
    { fieldLabelModifier = toLowerFirst . drop 3 }) ''RpcError)

data Filter = Filter
  { filterAddress   :: Maybe Address
  , filterTopics    :: Maybe [Maybe Text]
  , filterFromBlock :: Maybe Text
  , filterToBlock   :: Maybe Text
  } deriving Show

$(deriveJSON (defaultOptions
    { fieldLabelModifier = toLowerFirst . drop 6 }) ''Filter)

newtype FilterId = FilterId Int
  deriving (Show, Eq, Ord)

instance FromJSON FilterId where
    parseJSON (String v) =
        case R.hexadecimal v of
            Right (x, "") -> return (FilterId x)
            _ -> fail "Unable to parse FilterId!"
    parseJSON _ = fail "The string is required!"

instance ToJSON FilterId where
    toJSON (FilterId x) =
        let hexValue = B.toLazyText (B.hexadecimal x)
        in  toJSON ("0x" <> hexValue)

data Change = Change
  { changeLogIndex         :: Text
  , changeTransactionIndex :: Text
  , changeTransactionHash  :: Text
  , changeBlockHash        :: Text
  , changeBlockNumber      :: Text
  , changeAddress          :: Address
  , changeData             :: Text
  , changeTopics           :: [Text]
  } deriving Show

$(deriveJSON (defaultOptions
    { fieldLabelModifier = toLowerFirst . drop 6 }) ''Change)

data Call = Call
  { callFrom    :: Maybe Address
  , callTo      :: Address
  , callGas     :: Maybe Text
  , callPrice   :: Maybe Text
  , callValue   :: Maybe Text
  , callData    :: Maybe Text
  } deriving Show

$(deriveJSON (defaultOptions
    { fieldLabelModifier = toLowerFirst . drop 4 }) ''Call)

data CallMode = Latest | Pending
  deriving (Show, Eq)

instance ToJSON CallMode where
    toJSON = toJSON . toLowerFirst . show

-- TODO: Wrap
type TxHash = Text
