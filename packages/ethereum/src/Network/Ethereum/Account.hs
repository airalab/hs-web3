-- |
-- Module      :  Network.Ethereum.Account
-- Copyright   :  Aleksandr Krupenkin 2016-2021
-- License     :  Apache-2.0
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- In Etereun there are two types of accounts:
-- * Externally owned account (EOAs): an account controlled by a private key, and if you own the private key associated with the EOA you have the ability to send ether and messages from it.
-- * Contract: an account that has its own code, and is controlled by code.
--
-- This module exports different kinds of EOAs: default, node managed and local. Node managed accounts
-- use 'Personal' JSON-RPC API for signing transactions. Local account sign transaction locally and
-- use 'sendRawTransaction' method to export transaction to Ethereum network.
--

module Network.Ethereum.Account
    (
    -- * The @Account@ type
      Account(..)

    -- * Default node account
    , DefaultAccount

    -- * Unlockable node account
    , PersonalAccount
    , Personal(..)

    -- * Local key account
    , LocalKeyAccount
    , LocalKey(..)

    -- * Transaction parameterization function and lenses
    , withParam
    , to
    , value
    , gasLimit
    , gasPrice
    , block
    , account

    ) where

import           Network.Ethereum.Account.Class    (Account (..))
import           Network.Ethereum.Account.Default  (DefaultAccount)
import           Network.Ethereum.Account.Internal (account, block, gasLimit,
                                                    gasPrice, to, value,
                                                    withParam)
import           Network.Ethereum.Account.LocalKey (LocalKey (..),
                                                    LocalKeyAccount)
import           Network.Ethereum.Account.Personal (Personal (..),
                                                    PersonalAccount)
