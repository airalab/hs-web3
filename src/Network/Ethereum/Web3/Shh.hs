-- |
-- Module      :  Network.Ethereum.Web3.Shh
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unknown
--
-- Ethereum node JSON-RPC Whisper API methods.
--
module Network.Ethereum.Web3.Shh where

import           Data.Text                      (Text)
import           Network.Ethereum.Web3.JsonRpc
import           Network.Ethereum.Web3.Provider
import           Network.Ethereum.Web3.Types

-- | Returns whisper node information
info :: Provider a => Web3 a NodeInfo
{-# INLINE info #-}
info = remote "shh_info"

-- | Do post whisper message
post :: Provider a => ShhPost -> Web3 a Bool
{-# INLINE post #-}
post = remote "shh_post"

-- | Create whisper message filter
newMessageFilter :: Provider a => ShhFilter -> Web3 a ShhIdentity
{-# INLINE newMessageFilter #-}
newMessageFilter = remote "shh_newMessageFilter"

-- | Get whisper filter messages
getFilterMessages :: Provider a => ShhIdentity -> Web3 a [ShhMessage]
{-# INLINE getFilterMessages #-}
getFilterMessages = remote "shh_getFilterMessages"
