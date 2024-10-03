{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- |
-- Module      :  Network.Ethereum.Contract
-- Copyright   :  Aleksandr Krupenkin 2016-2024
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- Smart contract type class and utils. A contract in the sense of Solidity
-- is a collection of code (its functions) and data (its state) that resides
-- at a specific address on the Ethereum blockchain.
--

module Network.Ethereum.Contract where

import           Data.Proxy                       (Proxy)
import           Data.Text                        (Text)

import           Data.ByteArray.HexString         (HexString)
import           Data.Solidity.Prim.Address       (Address)
import           Network.Ethereum.Account.Class   (Account)
import           Network.Ethereum.Account.Safe    (safeConfirmations, safeSend)
import           Network.Ethereum.Api.Types       (receiptContractAddress)
import           Network.Ethereum.Contract.Method (Method)
import           Network.JsonRpc.TinyClient       (JsonRpc)

-- | Contract description type clase
class Contract a where
    -- | Contract Solidity ABI
    -- https://solidity.readthedocs.io/en/latest/abi-spec.html
    abi :: Proxy a -> Text

    -- | Contract bytecode as hex string
    bytecode :: Proxy a -> HexString

-- | Create new smart contract on blockchain
new :: (Account p t, JsonRpc m, Method a, Monad (t m))
    => a
    -- ^ Contract constructor
    -> t m (Either HexString (Maybe Address))
    -- ^ Address of deployed contract when transaction success;
    -- transaction hash in case of a timeout
new = fmap (mapRight receiptContractAddress) . safeSend safeConfirmations
  where
    mapRight f e =
        case e of
            Left x  -> Left x
            Right y -> Right $ f y
