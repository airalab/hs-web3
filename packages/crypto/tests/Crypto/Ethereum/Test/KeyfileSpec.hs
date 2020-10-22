{-# LANGUAGE OverloadedStrings #-}
module Crypto.Ethereum.Test.KeyfileSpec where

import           Data.Aeson               (eitherDecode)
import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Lazy     as L (ByteString)
import           Data.UUID.Types          (UUID)
import           Test.Hspec

import           Crypto.Ethereum.Keyfile
import           Data.ByteArray.HexString (HexString)

pbkdf2_test_keyfile :: L.ByteString
pbkdf2_test_keyfile = "\
\{\
\    \"crypto\" : {\
\        \"cipher\" : \"aes-128-ctr\",\
\        \"cipherparams\" : {\
\            \"iv\" : \"6087dab2f9fdbbfaddc31a909735c1e6\"\
\        },\
\        \"ciphertext\" : \"5318b4d5bcd28de64ee5559e671353e16f075ecae9f99c7a79a38af5f869aa46\",\
\        \"kdf\" : \"pbkdf2\",\
\        \"kdfparams\" : {\
\            \"c\" : 262144,\
\            \"dklen\" : 32,\
\            \"prf\" : \"hmac-sha256\",\
\            \"salt\" : \"ae3cd4e7013836a3df6bd7241b12db061dbe2c6785853cce422d148a624ce0bd\"\
\        },\
\        \"mac\" : \"517ead924a9d0dc3124507e3393d175ce3ff7c1e96529c6c555ce9e51205e9b2\"\
\    },\
\    \"id\" : \"3198bc9c-6672-5ab3-d995-4942343ae5b6\",\
\    \"version\" : 3\
\}"

scrypt_test_keyfile :: L.ByteString
scrypt_test_keyfile = "\
\{\
\    \"crypto\" : {\
\        \"cipher\" : \"aes-128-ctr\",\
\        \"cipherparams\" : {\
\            \"iv\" : \"83dbcc02d8ccb40e466191a123791e0e\"\
\        },\
\        \"ciphertext\" : \"d172bf743a674da9cdad04534d56926ef8358534d458fffccd4e6ad2fbde479c\",\
\        \"kdf\" : \"scrypt\",\
\        \"kdfparams\" : {\
\            \"dklen\" : 32,\
\            \"n\" : 262144,\
\            \"r\" : 1,\
\            \"p\" : 8,\
\            \"salt\" : \"ab0c7876052600dd703518d6fc3fe8984592145b591fc8fb5c6d43190334ba19\"\
\        },\
\        \"mac\" : \"2103ac29920d71da29f15d75b4a16dbe95cfd7ff8faea1056c33131d846e3097\"\
\    },\
\    \"id\" : \"3198bc9c-6672-5ab3-d995-4942343ae5b6\",\
\    \"version\" : 3\
\}"

test_uuid :: UUID
test_uuid = read "3198bc9c-6672-5ab3-d995-4942343ae5b6"

test_pbkdf2_mac :: HexString
test_pbkdf2_mac = "517ead924a9d0dc3124507e3393d175ce3ff7c1e96529c6c555ce9e51205e9b2"

test_scrypt_mac :: HexString
test_scrypt_mac = "2103ac29920d71da29f15d75b4a16dbe95cfd7ff8faea1056c33131d846e3097"

test_password :: ByteString
test_password = "testpassword"

test_private :: HexString
test_private = "7a28b5ba57c53603b0b07b56bba752f7784bf506fa95edc395f5cf6c7514fe9d"

spec :: Spec
spec = parallel $ do
    describe "AES-128-CTR and PBKDF2-SHA-256" $ do
        it "can decode keyfile" $
            case eitherDecode pbkdf2_test_keyfile of
                Left e     -> error e
                Right ekey -> do
                    encryptedKeyId ekey `shouldBe` test_uuid
                    encryptedKeyMac ekey `shouldBe` test_pbkdf2_mac

        it "can decrypt keyfile" $ do
            case eitherDecode pbkdf2_test_keyfile of
                Left e     -> error e
                Right ekey -> decrypt ekey test_password `shouldBe` Just test_private

    describe "AES-128-CTR and Scrypt" $ do
        it "can decode keyfile" $
            case eitherDecode scrypt_test_keyfile of
                Left e     -> error e
                Right ekey -> do
                    encryptedKeyId ekey `shouldBe` test_uuid
                    encryptedKeyMac ekey `shouldBe` test_scrypt_mac

        it "can decrypt keyfile" $
            case eitherDecode scrypt_test_keyfile of
                Left e     -> error e
                Right ekey -> decrypt ekey test_password `shouldBe` Just test_private
