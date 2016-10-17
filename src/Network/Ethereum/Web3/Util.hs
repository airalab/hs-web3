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

import Network.Ethereum.ContractABI (Method, signature)
import Network.Ethereum.Web3.Types (Web3, Filter(..))
import Network.Ethereum.Address (Address, toText)
import Network.Ethereum.Web3.API (web3_sha3)
import Network.Ethereum.Web3.Internal (hex)
import qualified Data.Text as T
import Data.Monoid ((<>))
import Data.Text (Text)

sha3Text :: Text -> Web3 Text
sha3Text = web3_sha3 . ("0x" <>) . hex

eventFilter :: Address -> Method -> Web3 Filter
eventFilter addr event = do
    topic0 <- sha3Text (signature event)
    return (Filter (Just ("0x" <> toText addr)) (Just [Just topic0, Nothing]) Nothing Nothing)

methodId :: Method -> Web3 Text
methodId = fmap (T.take 10) . sha3Text . signature
