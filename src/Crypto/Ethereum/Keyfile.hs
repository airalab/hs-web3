{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-- |
-- Module      :  Crypto.Ethereum.Keyfile
-- Copyright   :  Alexander Krupenkin 2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Web3 Secret Storage implementation.
-- Spec https://github.com/ethereum/wiki/wiki/Web3-Secret-Storage-Definition.
--

module Crypto.Ethereum.Keyfile
    (
    -- * Encrypted Ethereum private key
      EncryptedKey(..)
    , Cipher(..)
    , Kdf(..)

    -- * Secret storage packers
    , decrypt
    , encrypt
    ) where

import           Crypto.Cipher.AES        (AES128)
import           Crypto.Cipher.Types      (IV, cipherInit, ctrCombine, makeIV)
import           Crypto.Error             (throwCryptoError)
import qualified Crypto.KDF.PBKDF2        as Pbkdf2 (Parameters (..),
                                                     fastPBKDF2_SHA256)
import qualified Crypto.KDF.Scrypt        as Scrypt (Parameters (..), generate)
import           Crypto.Random            (MonadRandom (getRandomBytes))
import           Data.Aeson               (FromJSON (..), ToJSON (..), Value,
                                           object, withObject, (.:), (.=))
import           Data.Aeson.Types         (Parser)
import           Data.ByteArray           (ByteArray, ByteArrayAccess, convert)
import qualified Data.ByteArray           as BA (drop, take, unpack)
import           Data.Maybe               (fromJust)
import           Data.Text                (Text)
import           Data.UUID.Types          (UUID)
import           Data.UUID.Types.Internal (buildFromBytes)

import           Crypto.Ethereum.Utils    (sha3)
import           Data.ByteArray.HexString (HexString)

-- | Key derivation function parameters and salt.
data Kdf = Pbkdf2 !Pbkdf2.Parameters !HexString
         | Scrypt !Scrypt.Parameters !HexString

-- | Cipher parameters.
data Cipher = Aes128Ctr
    { cipherIv :: !(IV AES128), cipherText :: !HexString }

-- | Secret Storage representation on memory.
data EncryptedKey = EncryptedKey
  { encryptedKeyId      :: !UUID        -- ^ Random key ID
  , encryptedKeyVersion :: !Int         -- ^ Version (suppoted version 3 only)
  , encryptedKeyCipher  :: !Cipher      -- ^ Cipher (supported AES-128-CTR only)
  , encryptedKeyKdf     :: !Kdf         -- ^ Key derivation function
  , encryptedKeyMac     :: !HexString   -- ^ MAC
  }

instance Eq EncryptedKey where
    a == b = encryptedKeyId a == encryptedKeyId b

instance Show EncryptedKey where
    show EncryptedKey{..} = "EncryptedKey " ++ show encryptedKeyId

instance FromJSON EncryptedKey where
    parseJSON = encryptedKeyParser

instance ToJSON EncryptedKey where
    toJSON = encryptedKeyBuilder

encryptedKeyBuilder :: EncryptedKey -> Value
encryptedKeyBuilder EncryptedKey{..} = object
    [ "id"      .= encryptedKeyId
    , "version" .= encryptedKeyVersion
    , "crypto"  .= object
        [ "cipher"        .= cipherName encryptedKeyCipher
        , "cipherparams"  .= cipherParams encryptedKeyCipher
        , "ciphertext"    .= cipherText encryptedKeyCipher
        , "kdf"           .= kdfName encryptedKeyKdf
        , "kdfparams"     .= kdfParams encryptedKeyKdf
        , "mac"           .= encryptedKeyMac
        ]
    ]
  where
    cipherName :: Cipher -> Text
    cipherName Aes128Ctr{..} = "aes-128-ctr"

    cipherParams :: Cipher -> Value
    cipherParams Aes128Ctr{..} = object [ "iv" .= (convert cipherIv :: HexString) ]

    kdfName :: Kdf -> Text
    kdfName = \case
        Pbkdf2 _ _ -> "pbkdf2"
        Scrypt _ _ -> "scrypt"

    kdfParams :: Kdf -> Value
    kdfParams = \case
        Pbkdf2 params salt ->
            object [ "salt"  .= salt
                   , "dklen" .= Pbkdf2.outputLength params
                   , "c"     .= Pbkdf2.iterCounts params
                   ]
        Scrypt params salt ->
            object [ "salt"  .= salt
                   , "dklen" .= Scrypt.outputLength params
                   , "p"     .= Scrypt.p params
                   , "r"     .= Scrypt.r params
                   , "n"     .= Scrypt.n params
                   ]

encryptedKeyParser :: Value -> Parser EncryptedKey
encryptedKeyParser = withObject "EncryptedKey" $ \v -> do
    uuid    <- v .: "id"
    version <- v .: "version"
    crypto  <- v .: "crypto"
    cipher  <- parseCipher crypto
    kdf     <- parseKdf crypto
    mac     <- withObject "Crypto" (.: "mac") crypto
    return $ EncryptedKey uuid version cipher kdf mac

parseCipher :: Value -> Parser Cipher
parseCipher = withObject "Cipher" $ \v -> do
    name <- v .: "cipher"
    case name :: Text of
        "aes-128-ctr" -> do
            params <- v .: "cipherparams"
            hexiv <- params .: "iv"
            text <- v .: "ciphertext"
            case makeIV (hexiv :: HexString) of
                Just iv -> return (Aes128Ctr iv text)
                Nothing -> fail $ "Unable to make IV from " ++ show hexiv
        _ -> fail $ show name ++ " not implemented yet"

parseKdf :: Value -> Parser Kdf
parseKdf = withObject "Kdf" $ \v -> do
    name   <- v .: "kdf"
    params <- v .: "kdfparams"
    dklen  <- params .: "dklen"
    salt   <- params .: "salt"
    case name :: Text of
        "pbkdf2" -> do
            iterations <- params .: "c"
            prf <- params .: "prf"
            case prf :: Text of
                "hmac-sha256" -> return $ Pbkdf2 (Pbkdf2.Parameters iterations dklen) salt
                _             -> fail $ show prf ++ " not implemented yet"
        "scrypt" -> do
            p <- params .: "p"
            r <- params .: "r"
            n <- params .: "n"
            return $ Scrypt (Scrypt.Parameters n r p dklen) salt
        _ -> fail $ show name ++ " not implemented yet"

defaultKdf :: HexString -> Kdf
defaultKdf = Scrypt (Scrypt.Parameters n r p dklen)
  where
    dklen = 32
    n = 262144
    r = 1
    p = 8

deriveKey :: (ByteArrayAccess password, ByteArray ba) => Kdf -> password -> ba
deriveKey kdf password =
    case kdf of
        Pbkdf2 params salt -> Pbkdf2.fastPBKDF2_SHA256 params password salt
        Scrypt params salt -> Scrypt.generate params password salt


-- | Decrypt Ethereum private key.
--
-- Typically Web3 Secret Storage is JSON-encoded. 'EncryptedKey' data type has 'FromJSON' instance
-- to helps decode it from JSON-encoded string or file.
--
-- @
--   let decryptJSON pass = flip decrypt pass <=< decode
-- @
--
decrypt :: (ByteArrayAccess password, ByteArray privateKey)
         => EncryptedKey
         -> password
         -> Maybe privateKey
decrypt EncryptedKey{..} password
  | mac == encryptedKeyMac = Just (convert privateKey)
  | otherwise = Nothing
  where
    privateKey = ctrCombine cipher iv ciphertext
    cipher = throwCryptoError $ cipherInit (BA.take 16 derivedKey) :: AES128
    derivedKey = deriveKey encryptedKeyKdf password
    ciphertext = cipherText encryptedKeyCipher
    mac = sha3 (BA.drop 16 derivedKey <> ciphertext)
    iv  = cipherIv encryptedKeyCipher

-- | Encrypt Ethereum private key.
--
-- @
--   let encryptJSON pass key = encode <$> encrypt key pass
-- @
encrypt :: (ByteArray privateKey, ByteArrayAccess password, MonadRandom m)
        => privateKey
        -> password
        -> m EncryptedKey
encrypt privateKey password = do
    kdf <- defaultKdf <$> getRandomBytes 16
    iv <- randomIV
    let derivedKey = deriveKey kdf password
        cipher = throwCryptoError $ cipherInit (BA.take 16 derivedKey) :: AES128
        ciphertext = ctrCombine cipher iv privateKey
        mac = sha3 (BA.drop 16 derivedKey <> ciphertext)
    uuid <- randomUUID
    return $ EncryptedKey uuid 3 (Aes128Ctr iv $ convert ciphertext) kdf mac
  where
    randomUUID = do
        uuid <- getRandomBytes 16
        let bs = BA.unpack (uuid :: HexString)
        return $ buildFromBytes 4
            (head bs) (bs !! 1) (bs !! 2) (bs !! 3)
            (bs !! 4) (bs !! 5) (bs !! 6) (bs !! 7)
            (bs !! 8) (bs !! 9) (bs !! 10) (bs !! 11)
            (bs !! 12) (bs !! 13) (bs !! 14) (bs !! 15)
    randomIV = do
        iv <- getRandomBytes 16
        return $ fromJust $ makeIV (iv :: HexString)
