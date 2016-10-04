-- |
-- Module      :  Network.Ethereum.Web3.Utils
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  POSIX / WIN32
--
-- Web3 utility.
--
module Network.Ethereum.Web3.Util where

import Network.Ethereum.ContractAbi (Method, eventSignature)
import Network.Ethereum.Web3.Api (Filter(..), sha3)
import Network.Ethereum.Web3.Types (Web3)
import Data.HexString (toText, fromBytes)
import Data.Text.Encoding (encodeUtf8)
import Data.Monoid ((<>))
import Data.Text (Text)

type Address = Text

eventFilter :: Address -> Method -> Web3 Filter
eventFilter addr event = do
    topic0 <- sha3 (hex $ eventSignature event)
    return (Filter (Just addr) (Just [Just topic0, Nothing]) Nothing Nothing)
  where hex = ("0x" <>) . toText . fromBytes . encodeUtf8
