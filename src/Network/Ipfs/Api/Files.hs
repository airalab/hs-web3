{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Ipfs.Api.Files
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unknown
--
-- Api calls with `files` prefix.
--

module Network.Ipfs.Api.Files where

import           Control.Monad.IO.Class         (MonadIO)
import           Data.Text                      (Text, pack)
import           Network.HTTP.Client            (responseStatus)
import           Network.HTTP.Types             (Status (..))
import           Servant.API.ContentTypes       (NoContent)

import           Network.Ipfs.Api.Internal      (_filesChcid, _filesCp,
                                                 _filesFlush, _filesLs,
                                                 _filesMkdir, _filesMv,
                                                 _filesRead, _filesRm,
                                                 _filesStat)
import           Network.Ipfs.Api.Internal.Call (call, multipartCall)
import           Network.Ipfs.Api.Types         (FilesFlushObj, FilesLsObj,
                                                 FilesReadType, FilesStatObj)
import           Network.Ipfs.Client            (IpfsT)

-- | Change the cid version or hash function of the root node of a given mfsPath.
chcidVer :: MonadIO m => Text -> Int -> IpfsT m NoContent
chcidVer mfsPath = call . _filesChcid (Just mfsPath) . Just

-- | Copy files into mfs.
cp :: MonadIO m => Text -> Text -> IpfsT m NoContent
cp src = call . _filesCp (Just src) . Just

-- | Flush a given path's data to disk.
flush :: MonadIO m => Text -> IpfsT m FilesFlushObj
flush = call . _filesFlush . Just

-- | List directories in the local mutable namespace.
ls :: MonadIO m => Text -> IpfsT m FilesLsObj
ls = call . _filesLs . Just

-- | Make directories.
mkdir :: MonadIO m => Text -> IpfsT m NoContent
mkdir = call . _filesMkdir . Just

-- | Move files.
mv :: MonadIO m => Text -> Text -> IpfsT m NoContent
mv src = call . _filesMv (Just src) . Just

-- | Read a file in a given mfs.
read :: MonadIO m => Text -> IpfsT m FilesReadType
read = call . _filesRead . Just

-- | Display file status.
stat :: MonadIO m => Text -> IpfsT m FilesStatObj
stat = call . _filesStat . Just

-- | Remove a file.
filesRm :: MonadIO m => Text -> IpfsT m NoContent
filesRm = call . flip _filesRm (Just True) . Just

-- | Write to a mutable file in a given filesystem.
write :: MonadIO m => Text -> Text -> Bool -> IpfsT m Bool
write mfsPath filePath toTruncate = isSuccess <$> multipartCall uri filePath
  where
    uri = "files/write?arg=" <> mfsPath <> "&create=true" <> "&truncate=" <> (pack $ show toTruncate)
    isSuccess = (200 ==) . statusCode . responseStatus
