{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Ipfs.Api.Object
-- Copyright   :  Aleksandr Krupenkin 2016-2020
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unknown
--
-- Api calls with `object` prefix.
--

module Network.Ipfs.Api.Object where

import           Control.Monad.IO.Class         (MonadIO)
import           Data.Aeson                     (decode)
import           Data.Text                      (Text)
import           Network.HTTP.Client            (responseBody)

import           Network.Ipfs.Api.Internal      (_objectAddLink, _objectData,
                                                 _objectDiff, _objectGet,
                                                 _objectGetLinks, _objectNew,
                                                 _objectRmLink, _objectStat)
import           Network.Ipfs.Api.Internal.Call (call, multipartCall)
import           Network.Ipfs.Api.Types         (ObjectDiffObj, ObjectGetObj,
                                                 ObjectLinksObj, ObjectObj,
                                                 ObjectReturnType,
                                                 ObjectStatObj)
import           Network.Ipfs.Client            (IpfsT)

-- | Output the raw bytes of an IPFS object.
object :: MonadIO m => Text -> IpfsT m ObjectReturnType
object = call . _objectData

-- | Create a new object from an ipfs template.
new :: MonadIO m => IpfsT m ObjectObj
new = call _objectNew

-- | Output the links pointed to by the specified object.
getLinks :: MonadIO m => Text -> IpfsT m ObjectLinksObj
getLinks = call . _objectGetLinks

-- | Add a Merkle-link to the given object and return the hash of the result.
addLink :: MonadIO m => Text -> Text -> Text -> IpfsT m ObjectLinksObj
addLink hash name = call . _objectAddLink hash (Just name) . Just

-- | Remove a Merkle-link from the given object and return the hash of the result.
rmLink :: MonadIO m => Text -> Text -> IpfsT m ObjectLinksObj
rmLink key = call . _objectRmLink key . Just

-- | Append data to what already exists in the data segment in the given object.
appendData :: MonadIO m => Text -> Text -> IpfsT m (Maybe ObjectLinksObj)
appendData key = fmap decodeResponse . multipartCall ("object/patch/append-data?arg=" <> key)
  where
    decodeResponse = decode . responseBody

-- | Set the data field of an IPFS object.
setData :: MonadIO m => Text -> Text -> IpfsT m (Maybe ObjectLinksObj)
setData key = fmap decodeResponse . multipartCall ("object/patch/set-data?arg=" <> key)
  where
    decodeResponse = decode . responseBody

-- | Get and serialize the DAG node named by key.
get :: MonadIO m => Text -> IpfsT m ObjectGetObj
get = call . _objectGet

-- | 'Display the diff between two ipfs objects.
diff :: MonadIO m => Text -> Text -> IpfsT m ObjectDiffObj
diff firstKey = call . _objectDiff firstKey . Just

-- | Store input as a DAG object, print its key.
put :: MonadIO m => Text -> IpfsT m (Maybe ObjectObj)
put = fmap decodeResponse . multipartCall "object/put"
  where
    decodeResponse = decode . responseBody

-- | Get stats for the DAG node named by key.
objectStat :: MonadIO m => Text -> IpfsT m ObjectStatObj
objectStat = call . _objectStat
