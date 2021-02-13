{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Network.Ethereum.Transaction
-- Copyright   :  Aleksandr Krupenkin 2016-2021
--                Roy Blankman 2018
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- Transaction managing utils.
--

module Network.Ethereum.Transaction where

import           Data.ByteArray             (ByteArray, convert)
import           Data.ByteString            (ByteString, empty)
import           Data.Maybe                 (fromJust, fromMaybe)
import           Data.RLP                   (packRLP, rlpEncode)
import           Data.Word                  (Word8)

import           Data.ByteArray.HexString   (toBytes)
import           Data.Solidity.Prim.Address (toHexString)
import           Network.Ethereum.Api.Types (Call (..), Quantity (unQuantity))
import           Network.Ethereum.Unit      (Shannon, toWei)

-- | Ethereum transaction packer.
--
-- Two way RLP encoding of Ethereum transaction: for unsigned and signed.
-- Packing scheme described in https://github.com/ethereum/EIPs/blob/master/EIPS/eip-155.md
encodeTransaction :: ByteArray ba
                  => Call
                  -- ^ Transaction call
                  -> Integer
                  -- ^ Chain ID
                  -> Maybe (Integer, Integer, Word8)
                  -- ^ Should contain signature when transaction signed
                  -> ba
                  -- ^ RLP encoded transaction
encodeTransaction Call{..} chain_id rsv =
    let (to       :: ByteString) = maybe mempty (toBytes . toHexString) callTo
        (value    :: Integer)    = unQuantity $ fromJust callValue
        (nonce    :: Integer)    = unQuantity $ fromJust callNonce
        (gasPrice :: Integer)    = maybe defaultGasPrice unQuantity callGasPrice
        (gasLimit :: Integer)    = unQuantity $ fromJust callGas
        (input    :: ByteString) = convert $ fromMaybe mempty callData

    in convert . packRLP $ case rsv of
        -- Unsigned transaction by EIP155
        Nothing        -> rlpEncode (nonce, gasPrice, gasLimit, to, value, input, chain_id, empty, empty)
        -- Signed transaction
        Just (r, s, v) ->
            let v' = fromIntegral v + 8 + 2 * chain_id  -- Improved 'v' according to EIP155
             in rlpEncode (nonce, gasPrice, gasLimit, to, value, input, v', r, s)
  where
    defaultGasPrice = toWei (10 :: Shannon)
