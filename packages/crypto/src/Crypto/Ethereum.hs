-- |
-- Module      :  Crypto.Ethereum
-- Copyright   :  Aleksandr Krupenkin 2016-2024
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- Ethereum cryptography primitives.
--

module Crypto.Ethereum
    (
    -- * ECDSA crypto key ops
      PrivateKey
    , PublicKey
    , importKey
    , derivePubKey

    -- * Digital Signature Algorithm
    , signMessage

    -- * Hash function
    , keccak256
    ) where

import           Crypto.Ecdsa.Utils        (derivePubKey, importKey)
import           Crypto.Ethereum.Signature (signMessage)
import           Crypto.Ethereum.Utils     (keccak256)
import           Crypto.PubKey.ECC.ECDSA   (PrivateKey, PublicKey)
