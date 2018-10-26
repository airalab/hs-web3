{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Ethereum.Api.Shh
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unknown
--
-- Ethereum node JSON-RPC Whisper API methods.
--

module Network.Ethereum.Api.Shh where

import           Data.Text                  (Text)

import           Data.ByteArray.HexString   (HexString)
import           Network.Ethereum.Api.Types (NodeInfo, ShhFilter, ShhMessage,
                                             ShhPost)
import           Network.JsonRpc.TinyClient (JsonRpc (..))

-- | Returns whisper node version
version :: JsonRpc m => m Text
{-# INLINE version #-}
version = remote "shh_version"

-- | Returns whisper node information
info :: JsonRpc m => m NodeInfo
{-# INLINE info #-}
info = remote "shh_info"

-- | Do post whisper message
post :: JsonRpc m => ShhPost -> m Bool
{-# INLINE post #-}
post = remote "shh_post"

-- | Create whisper message filter
newMessageFilter :: JsonRpc m => ShhFilter -> m HexString
{-# INLINE newMessageFilter #-}
newMessageFilter = remote "shh_newMessageFilter"

-- | Get whisper filter messages
getFilterMessages :: JsonRpc m => HexString -> m [ShhMessage]
{-# INLINE getFilterMessages #-}
getFilterMessages = remote "shh_getFilterMessages"
