{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Polkadot.Api.Author
-- Copyright   :  Aleksandr Krupenkin 2016-2020
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Polkadot RPC methods with `author` prefix.
--

module Network.Polkadot.Api.Author where

import           Data.Text                  (Text)

import           Data.ByteArray.HexString   (HexString)
import           Network.JsonRpc.TinyClient (JsonRpc (..))

-- | Returns true if the keystore has private keys for the given public key and key type.
hasKey :: JsonRpc m
       => HexString
       -- ^ Public key
       -> Text
       -- ^ Key type
       -> m Bool
{-# INLINE hasKey #-}
hasKey = remote "author_hasKey"

-- | Returns true if the keystore has private keys for the given session public keys.
hasSessionKeys :: JsonRpc m
               => HexString
               -- ^ Session keys
               -> m Bool
{-# INLINE hasSessionKeys #-}
hasSessionKeys = remote "author_hasSessionKeys"

-- | Insert a key into the keystore.
insertKey :: JsonRpc m
          => Text
          -- ^ Key type
          -> Text
          -- ^ Key secret URI
          -> HexString
          -- ^ Public key
          -> m HexString
{-# INLINE insertKey #-}
insertKey = remote "author_insertKey"

-- | Returns all pending extrinsics, potentially grouped by sender.
pendingExtrinsics :: JsonRpc m => m [HexString]
{-# INLINE pendingExtrinsics #-}
pendingExtrinsics = remote "author_pendingExtrinsics"

-- | Remove given extrinsic from the pool and temporarily ban it to prevent reimporting.
removeExtrinsic :: JsonRpc m
                => [HexString]
                -- ^ Extrinsic or hash
                -> m HexString
{-# INLINE removeExtrinsic #-}
removeExtrinsic = remote "author_removeExtrinsic"

-- | Generate new session keys and returns the corresponding public keys.
rotateKeys :: JsonRpc m => m HexString
{-# INLINE rotateKeys #-}
rotateKeys = remote "author_rotateKeys"

-- | Submit a fully formatted extrinsic for block inclusion.
submitExtrinsic :: JsonRpc m
                => HexString
                -- ^ Extrinsic
                -> m HexString
                -- ^ Hash
{-# INLINE submitExtrinsic #-}
submitExtrinsic = remote "author_submitExtrinsic"
