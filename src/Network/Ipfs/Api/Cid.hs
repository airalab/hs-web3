-- |
-- Module      :  Network.Ipfs.Api.Cid
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unknown
--
-- Api calls with `cid` prefix.
--

module Network.Ipfs.Api.Cid where

import           Control.Monad.IO.Class         (MonadIO)
import           Data.Text                      (Text)

import           Network.Ipfs.Api.Internal      (_cidBase32, _cidBases,
                                                 _cidCodecs, _cidFormat,
                                                 _cidHashes)
import           Network.Ipfs.Api.Internal.Call (call)
import           Network.Ipfs.Api.Types         (CidBasesObj, CidCodecsObj,
                                                 CidHashesObj, CidObj)
import           Network.Ipfs.Client            (IpfsT)

-- | List available multibase encodings.
bases :: MonadIO m => IpfsT m [CidBasesObj]
bases = call _cidBases

-- | List available CID codecs.
codecs :: MonadIO m => IpfsT m [CidCodecsObj]
codecs = call _cidCodecs

-- | List available multihashes.
hashes :: MonadIO m => IpfsT m [CidHashesObj]
hashes = call _cidHashes

-- | Convert CIDs to Base32 CID version 1.
base32 :: MonadIO m => Text -> IpfsT m CidObj
base32 = call . _cidBase32

-- | Format and convert a CID in various useful ways.
format :: MonadIO m => Text -> IpfsT m CidObj
format = call . _cidFormat
