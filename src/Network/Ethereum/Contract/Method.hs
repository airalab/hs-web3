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

import           Control.Monad.Catch               (throwM)
import           Control.Monad.Reader
import           Data.Monoid                       ((<>))
import           Data.Proxy                        (Proxy (..))

import           Network.Ethereum.ABI.Class        (ABIGet, ABIPut, ABIType (..))
import           Network.Ethereum.ABI.Codec        (decode, encode)
import           Network.Ethereum.ABI.Prim.Bytes   (Bytes)
import qualified Network.Ethereum.Web3.Eth         as Eth
import           Network.Ethereum.Web3.Provider    (Provider (..), SigningConfiguration (..),
                                                    Web3, Web3Error (ParserFail, UserFail))
import           Network.Ethereum.Web3.Transaction (createRawTransaction)
import           Network.Ethereum.Web3.Types       (Call (callData), DefaultBlock,
                                                    Hash)

class ABIPut a => Method a where
  selector :: Proxy a -> Bytes

instance ABIType () where
  isDynamic _ = False

instance ABIPut ()

-- | Send transaction without method selection
instance Method () where
  selector = mempty

-- | 'sendTx' is used to submit a state changing transaction.
sendTx :: Method a
       => Call
       -- ^ Call configuration
       -> a
       -- ^ method data
       -> Web3 Hash
sendTx call' (dat :: a) = do
    let sel = selector (Proxy :: Proxy a)
    signingConfigM <- asks (signingConfiguration . fst)
    case signingConfigM of
        Just (SigningConfiguration privKey chainId) -> do
            txBytes <- either (throwM . UserFail) pure $
                createRawTransaction (call' { callData = Just $ sel <> encode dat }) chainId privKey
            Eth.sendRawTransaction txBytes
        Nothing -> Eth.sendTransaction (call' { callData = Just $ sel <> encode dat })

-- | 'call' is used to call contract methods that have no state changing effects.
call :: (Method a, ABIGet b)
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
        c = (call' { callData = Just $ sel <> encode dat })
    res <- Eth.call c mode
    case decode res of
        Left e  -> throwM $ ParserFail $ "Unable to parse response: " ++ e
        Right x -> return x
