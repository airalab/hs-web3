-- |
-- Module      :  Crypto.Ethereum
-- Copyright   :  Alexander Krupenkin 2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- Ethereum ECC support module.
--

module Crypto.Ethereum
    (
    -- * Ethereum crypto key ops
      PrivateKey
    , PublicKey
    , importKey
    , derivePubKey

    -- * Digital Signature Algorithm
    , signMessage

    -- * Hash function
    , sha3
    ) where

import           Crypto.Ethereum.Signature (signMessage)
import           Crypto.Ethereum.Utils     (derivePubKey, importKey, sha3)
import           Crypto.PubKey.ECC.ECDSA   (PrivateKey, PublicKey)
