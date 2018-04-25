{-# LANGUAGE OverloadedStrings #-}

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

import           Network.Ethereum.ABI.Prim.Bytes (Bytes)
import           Network.Ethereum.Web3.Provider  (Web3)
import           Network.Ethereum.Web3.Types     (NodeInfo, ShhFilter,
                                                  ShhMessage, ShhPost)
import           Network.JsonRpc.TinyClient      (remote)

-- | Returns whisper node information
info :: Web3 NodeInfo
{-# INLINE info #-}
info = remote "shh_info"

-- | Do post whisper message
post :: ShhPost -> Web3 Bool
{-# INLINE post #-}
post = remote "shh_post"

-- | Create whisper message filter
newMessageFilter :: ShhFilter -> Web3 Bytes
{-# INLINE newMessageFilter #-}
newMessageFilter = remote "shh_newMessageFilter"

-- | Get whisper filter messages
getFilterMessages :: Bytes -> Web3 [ShhMessage]
{-# INLINE getFilterMessages #-}
getFilterMessages = remote "shh_getFilterMessages"
