{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      :  Network.Ipfs.Api.Api
-- Copyright   :  Alexander Krupenkin 2016-2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Ipfs API provider.
--

module Network.Ipfs.Api.Api where

import           Control.Arrow                    (left)
import           Control.Error                    (fmapL)
import           Control.Monad
import           Data.Aeson
import           Data.Int
import           Data.ByteString.Lazy             (toStrict)
import qualified Data.ByteString.Lazy.Char8()
import           Data.Proxy           
import qualified Data.Text                        as TextS
import qualified Data.Text.Encoding               as TextS
import           Data.Typeable            
import qualified Data.Vector                      as Vec (fromList,Vector)
import           Network.HTTP.Client()
import qualified Network.HTTP.Media               as M ((//))
import           Servant.API
import           Servant.Client


type CatReturnType = TextS.Text


data DirLink = DirLink
    {   name          :: String 
    ,   hash          :: String
    ,   size          :: Int64
    ,   contentType   :: Int
    ,   target        :: String
    } deriving (Show)
 
data DirObject = DirObject
    {   objectHash :: String
    ,   links      :: [DirLink] 
    } deriving (Show)

data LsObject = LsObject {  objects :: [DirObject]  } deriving (Show)


data RefsObject = RefsObject String deriving (Show)
{--    {   error :: String
    ,   ref   :: String 
    } deriving (Show)
--}

instance FromJSON DirLink where
    parseJSON (Object o) =
        DirLink  <$> o .: "Name"
                 <*> o .: "Hash"
                 <*> o .: "Size"
                 <*> o .: "Type"
                 <*> o .: "Target"
    
    parseJSON _ = mzero

instance FromJSON DirObject where
    parseJSON (Object o) =
        DirObject  <$> o .: "Hash"
                   <*> o .: "Links"
    
    parseJSON _ = mzero

instance FromJSON LsObject where
    parseJSON (Object o) =
        LsObject  <$> o .: "Objects"

    parseJSON _ = mzero
{--
instance FromJSON RefsObject where
    parseJSON (Object o) =
        RefsObject  <$> o .: "Err"
                    <*> o .: "Ref"

    parseJSON _ = mzero
--}
-- | Defining a content type same as PlainText without charset
data IpfsText deriving Typeable

instance Servant.API.Accept IpfsText where
    contentType _ = "text" M.// "plain"

-- | @left show . TextS.decodeUtf8' . toStrict@
instance MimeUnrender IpfsText TextS.Text where
    mimeUnrender _ = left show . TextS.decodeUtf8' . toStrict

instance {-# OVERLAPPING #-} MimeUnrender JSON (Vec.Vector RefsObject) where
  mimeUnrender _ bs = do
    t <- fmapL show (TextS.decodeUtf8' (toStrict bs))
    pure (Vec.fromList (map RefsObject (lines $ TextS.unpack t)))    

type IpfsApi = "cat" :> Capture "cid" String :> Get '[IpfsText] CatReturnType
            :<|> "ls" :> Capture "cid" String :> Get '[JSON] LsObject
            :<|> "refs" :> Capture "cid" String :> Get '[JSON] (Vec.Vector RefsObject)
            :<|> "refs" :> "local" :> Get '[JSON] (Vec.Vector RefsObject)

ipfsApi :: Proxy IpfsApi
ipfsApi =  Proxy

_cat :: String -> ClientM CatReturnType
_ls :: String -> ClientM LsObject
_refs :: String -> ClientM (Vec.Vector RefsObject)
_refsLocal :: ClientM (Vec.Vector RefsObject) 

_cat :<|> _ls :<|> _refs :<|> _refsLocal = client ipfsApi
