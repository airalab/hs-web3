module Network.Ethereum.Web3.Logging
  ( Web3Logger(..)
  , Web3LogMessage(..)
  , noopLogger
  ) where

import           Data.Aeson           (Value)
import qualified Data.ByteString.Lazy as L

data Web3LogMessage = W3LMJsonRPCRequest Int Value
                    | W3LMJsonRPCRawResponse Int L.ByteString
                    | W3LMTrace String

newtype Web3Logger = Web3Logger { unWeb3Logger :: Web3LogMessage -> IO () }

noopLogger :: Web3Logger
noopLogger = Web3Logger . const $ return ()

-- These are to allow Provider to be Eq/Show
instance Show Web3Logger where
  show = const "Web3Logger"

instance Eq Web3Logger where
  _ == _ = True