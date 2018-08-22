{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Network.Ethereum.Contract.Method
-- Copyright   :  Alexander Krupenkin 2016-2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- Ethereum contract method support.
--

module Network.Ethereum.Contract.Method (
    Method(..)
  , call
  , sendTx
  ) where

import           Control.Monad.Catch           (throwM)
import           Data.ByteArray                (convert)
import           Data.Monoid                   ((<>))
import           Data.Proxy                    (Proxy (..))

import           Data.HexString                (HexString)
import           Data.Solidity.Abi             (AbiGet, AbiPut, AbiType (..))
import           Data.Solidity.Abi.Codec       (decode, encode)
import           Data.Solidity.Prim.Bytes      (Bytes)
import qualified Network.Ethereum.Api.Eth      as Eth
import           Network.Ethereum.Api.Provider (Web3, Web3Error (ParserFail))
import           Network.Ethereum.Api.Types    (Call (callData), DefaultBlock)

class AbiPut a => Method a where
  selector :: Proxy a -> Bytes

instance AbiType () where
  isDynamic _ = False

instance AbiPut ()

-- | Send transaction without method selection
instance Method () where
  selector = mempty

-- | 'sendTx' is used to submit a state changing transaction.
sendTx :: Method a
       => Call
       -- ^ Call configuration
       -> a
       -- ^ method data
       -> Web3 HexString
sendTx call' (dat :: a) =
    let sel = selector (Proxy :: Proxy a)
     in Eth.sendTransaction (call' { callData = Just $ convert sel <> encode dat })

-- | 'call' is used to call contract methods that have no state changing effects.
call :: (Method a, AbiGet b)
     => Call
     -- ^ Call configuration
     -> DefaultBlock
     -- ^ State mode for constant call (latest or pending)
     -> a
     -- ^ Method data
     -> Web3 b
     -- ^ 'Web3' wrapped result
call call' mode (dat :: a) = do
    let sel = selector (Proxy :: Proxy a)
    res <- Eth.call (call' { callData = Just $ convert sel <> encode dat }) mode
    case decode res of
        Left e  -> throwM $ ParserFail $ "Unable to parse response: " ++ e
        Right x -> return x
