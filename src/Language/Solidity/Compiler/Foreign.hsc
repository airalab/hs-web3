{-# LANGUAGE CPP, ForeignFunctionInterface #-}

-- |
-- Module      :  Language.Solidity.Compiler.Foreign
-- Copyright   :  Alexander Krupenkin 2017-2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- Ethereum Solidity library FFI.
--

module Language.Solidity.Compiler.Foreign where

#include <solidity_lite.h>

import Data.Map as M
import Data.ByteString.Char8 as BS
import Data.Typeable (Typeable)
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr)
import Foreign (free)

data Solidity
  deriving Typeable

foreign import ccall unsafe "create" c_create
    :: IO (Ptr Solidity)
foreign import ccall unsafe "destroy" c_destroy
    :: Ptr Solidity -> IO ()
foreign import ccall unsafe "addSource" c_addSource
    :: Ptr Solidity -> CString -> CString -> IO CInt
foreign import ccall unsafe "addLibrary" c_addLibrary
    :: Ptr Solidity -> CString -> CString -> IO CInt
foreign import ccall unsafe "compile" c_compile
    :: Ptr Solidity -> CInt -> IO CInt
foreign import ccall unsafe "abi" c_abi
    :: Ptr Solidity -> CString -> IO CString
foreign import ccall unsafe "binary" c_binary
    :: Ptr Solidity -> CString -> IO CString
foreign import ccall unsafe "errors" c_errors
    :: Ptr Solidity -> IO CString

withSolidity :: (Ptr Solidity -> IO a) -> IO a
{-# INLINE withSolidity #-}
withSolidity f = do
    ptr <- c_create
    r <- f ptr
    c_destroy ptr
    return r

withCStringPair :: (CString -> CString -> IO a) -> (ByteString, ByteString) -> IO a
{-# INLINE withCStringPair #-}
withCStringPair f (a, b) =
    withCString (BS.unpack a) $ \astr ->
        withCString (BS.unpack b) $ \bstr ->
            f astr bstr

addSources :: Ptr Solidity -> Map ByteString ByteString -> IO ()
{-# INLINE addSources #-}
addSources ptr = mapM_ (withCStringPair $ c_addSource ptr) . M.toList

addLibraries :: Ptr Solidity -> Map ByteString ByteString -> IO ()
{-# INLINE addLibraries #-}
addLibraries ptr = mapM_ (withCStringPair $ c_addLibrary ptr) . M.toList

result :: Ptr Solidity -> ByteString -> IO (ByteString, ByteString)
result ptr name =
    withCString (BS.unpack name) $ \cname -> do
        abi <- c_abi ptr cname
        bin <- c_binary ptr cname
        res <- (,) <$> fmap BS.pack (peekCString abi)
                   <*> fmap BS.pack (peekCString bin)
        free abi
        free bin
        return res

errors :: Ptr Solidity -> IO ByteString
errors ptr = do
    errs <- c_errors ptr
    res <- BS.pack <$> peekCString errs
    free errs
    return res

compile :: Map ByteString ByteString
        -> Map ByteString ByteString
        -> Bool
        -> IO (Either ByteString (Map ByteString (ByteString, ByteString)))
compile srcs libs opt =
    withSolidity $ \ptr -> do
        addSources ptr srcs
        addLibraries ptr libs
        res <- c_compile ptr optI
        let compiled = sequence $ M.mapWithKey (const . result ptr) srcs
        if res < 0
           then Left <$> errors ptr
           else Right <$> compiled
  where optI | opt = 1
             | otherwise = 0
