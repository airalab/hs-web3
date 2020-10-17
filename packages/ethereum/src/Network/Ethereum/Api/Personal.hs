{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Ethereum.Api.Personal
-- Copyright   :  Keagan McClelland 2018
-- License     :  Apache-2.0
--
-- Maintainer  :  keagan.mcclelland@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Ethereum node JSON-RPC API methods with `personal_` prefix.
--

module Network.Ethereum.Api.Personal where

import           Data.ByteArray.HexString   (HexString)
import           Data.Solidity.Prim.Address (Address)
import           Data.Text                  (Text)
import           Network.Ethereum.Api.Types (Call)
import           Network.JsonRpc.TinyClient (JsonRpc (..))

type Passphrase = Text

-- | Imports the given unencrypted private key (hex string) into the key store, encrypting it with the passphrase.
--
-- Parameters:
--
-- 1. unencrypted private key
--
-- 2. passphrase
--
-- Returns: address of new account
importRawKey :: JsonRpc m => HexString -> Passphrase -> m Address
{-# INLINE importRawKey #-}
importRawKey = remote "personal_importRawKey"

-- | Returns all the Ethereum account addresses of all keys in the key store.
listAccounts :: JsonRpc m => m [Address]
{-# INLINE listAccounts #-}
listAccounts = remote "personal_listAccounts"

-- | Removes the private key with given address from memory. The account can no longer be used to send transactions.
lockAccount :: JsonRpc m => Address -> m Bool
{-# INLINE lockAccount #-}
lockAccount = remote "personal_lockAccount"

-- | Generates a new private key and stores it in the key store directory. The key file is encrypted with the given
-- passphrase. Returns the address of the new account.
newAccount :: JsonRpc m => Passphrase -> m Address
{-# INLINE newAccount #-}
newAccount = remote "personal_newAccount"

-- | Decrypts the key with the given address from the key store.
--
-- The unencrypted key will be held in memory until it is locked again
--
-- The account can be used with eth_sign and eth_sendTransaction while it is unlocked.
unlockAccount :: JsonRpc m => Address -> Passphrase -> m Bool
{-# INLINE unlockAccount #-}
unlockAccount = remote "personal_unlockAccount"

-- | Validate the given passphrase and submit transaction.
--
-- The transaction is the same argument as for eth_sendTransaction and contains the from address. If the passphrase can
-- be used to decrypt the private key belonging to the transaction 'callFrom', the transaction is verified, signed and
-- send onto the network. The account is not unlocked globally in the node and cannot be used in other RPC calls.
sendTransaction :: JsonRpc m => Call -> Passphrase -> m HexString
{-# INLINE sendTransaction #-}
sendTransaction = remote "personal_sendTransaction"

-- | Returns an Ethereum specific signature with:
--
-- sign(keccak256("\x19Ethereum Signed Message:\n" + len(message) + message))).
--
-- when given a passphrase to decrypt the account's private key
sign :: JsonRpc m => HexString -> Address -> Passphrase -> m HexString
{-# INLINE sign #-}
sign = remote "personal_sign"

-- | Recovers address given message and signature data
--
-- Parameters:
--
-- 1. message: DATA, n bytes
--
-- 2. signature: DATA, 65 bytes
--
-- Returns: Address
ecRecover :: JsonRpc m => HexString -> HexString -> m Address
{-# INLINE ecRecover #-}
ecRecover = remote "personal_ecRecover"
