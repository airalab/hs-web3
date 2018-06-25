{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Ethereum.Web3.Personal
-- Copyright   :  Keagan McClelland 2018
-- License     :  BSD3
--
-- Maintainer  :  keagan.mcclelland@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Ethereum node JSON-RPC API methods with `personal_` prefix.
--

module Network.Ethereum.Web3.Personal where

import           Data.Text                         (Text)
import           Network.Ethereum.ABI.Prim.Address (Address)
import           Network.Ethereum.ABI.Prim.Bytes   (Bytes, BytesN)
import           Network.Ethereum.Web3.Provider    (Web3)
import           Network.Ethereum.Web3.Types       (Call, Hash)
import           Network.JsonRpc.TinyClient        (remote)

-- | Imports the given unencrypted private key (hex string) into the key store, encrypting it with the passphrase.
--
-- Parameters:
--
-- 1. unencrypted private key
--
-- 2. passphrase
--
-- Returns: address of new account
importRawKey :: BytesN 32 -> Text -> Web3 Address
{-# INLINE importRawKey #-}
importRawKey = remote "personal_importRawKey"

-- | Returns all the Ethereum account addresses of all keys in the key store.
listAccounts :: Web3 [Address]
{-# INLINE listAccounts #-}
listAccounts = remote "personal_listAccounts"

-- | Removes the private key with given address from memory. The account can no longer be used to send transactions.
lockAccount :: Address -> Web3 Bool
{-# INLINE lockAccount #-}
lockAccount = remote "personal_lockAccount"

-- | Generates a new private key and stores it in the key store directory. The key file is encrypted with the given 
-- passphrase. Returns the address of the new account.
newAccount :: Text -> Web3 Address
{-# INLINE newAccount #-}
newAccount = remote "personal_newAccount"

-- | Decrypts the key with the given address from the key store.
--
-- The unencrypted key will be held in memory until it is locked again
--
-- The account can be used with eth_sign and eth_sendTransaction while it is unlocked.
unlockAccount :: Address -> Text -> Web3 Bool
{-# INLINE unlockAccount #-}
unlockAccount = remote "personal_unlockAccount"

-- | Validate the given passphrase and submit transaction.
--
-- The transaction is the same argument as for eth_sendTransaction and contains the from address. If the passphrase can
-- be used to decrypt the private key belonging to the transaction 'callFrom', the transaction is verified, signed and 
-- send onto the network. The account is not unlocked globally in the node and cannot be used in other RPC calls.
sendTransaction :: Call -> Text -> Web3 Hash
{-# INLINE sendTransaction #-}
sendTransaction = remote "personal_sendTransaction"

-- | Returns an Ethereum specific signature with:
--
-- sign(keccak256("\x19Ethereum Signed Message:\n" + len(message) + message))).
--
-- when given a passphrase to decrypt the account's private key
sign :: Bytes -> Address -> Text -> Web3 Bytes
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
ecRecover :: Bytes -> Bytes -> Web3 Address
{-# INLINE ecRecover #-}
ecRecover = remote "personal_ecRecover"
