{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

-- |
-- Module      :  Network.Polkadot.Call
-- Copyright   :  Aleksandr Krupenkin 2016-2021
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
--

module Network.Polkadot.Call where

import           Codec.Scale                   (Decode, Encode, Generic, decode)
import           Data.List                     (findIndex)
import           Data.Text                     (Text)
import           Data.Word                     (Word8)
import qualified GHC.Generics                  as GHC (Generic)
import           Network.JsonRpc.TinyClient    (JsonRpc)

import           Network.Polkadot.Metadata     (MetadataVersioned (V13),
                                                metadata)
import           Network.Polkadot.Metadata.V13 (moduleCalls, moduleName,
                                                modules)
import           Network.Polkadot.Metadata.V9  (functionName)
import           Network.Polkadot.Rpc.State    (getMetadata)

-- | Call function of module using standard substrate extrionsic.
data Call a = Call !Word8 !Word8 !a
    deriving (Eq, Show, Generic, GHC.Generic, Encode, Decode)

-- | Create 'Call' type from text-encoded module and function.
new_call :: (Encode a, Decode a, JsonRpc m, MonadFail m)
         => Text
         -- ^ Module name.
         -> Text
         -- ^ Function name.
         -> a
         -- ^ Tuple of arguments.
         -> m (Call a)
new_call modName funName args = do
    Right (V13 meta) <- (fmap metadata . decode) <$> getMetadata
    case findIndex ((modName ==) . moduleName) (modules meta) of
        Nothing -> fail $ "module " <> show modName <> " not found"
        Just modIx ->
            case findIndex ((funName ==) . functionName) =<< moduleCalls (modules meta !! modIx) of
                Nothing -> fail $ "function " <> show funName <> " not found"
                Just funIx -> return $ Call (fromIntegral modIx) (fromIntegral funIx) args
