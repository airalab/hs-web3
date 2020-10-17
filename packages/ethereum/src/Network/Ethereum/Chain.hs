-- |
-- Module      :  Network.Ethereum.Chain
-- Copyright   :  Aleksandr Krupenkin 2016-2020
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- Ethereum chain IDs.
--

module Network.Ethereum.Chain where

-- | Ethereum mainnet CHAIN_ID from EIP155
foundation :: Integer
foundation = 1

-- | Ethereum testnet CHAIN_ID from EIP155
ropsten :: Integer
ropsten = 3

-- | Rokenby CHAIN_ID from EIP155
rikenby :: Integer
rikenby = 4

-- | Kovan CHAIN_ID from EIP155
kovan :: Integer
kovan = 42

-- | Ethereum Classic mainnet CHAIN_ID from EIP155
classic :: Integer
classic = 61

-- | Ethereum Classic testnet CHAIN_ID from EIP155
classicTestnet :: Integer
classicTestnet = 62
