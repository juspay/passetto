{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DerivingStrategies #-}

{- | Primitives and encryption logic.

The supposed scenarios are the following:

1. Master key can be encrypted using password;
2. Master key can be used to encrypt plain keys;
3. Plain keys are used to encrypt used-supplied data.

This module tends to use only safe primitives provided by libsodium.

Note, that before using methods of this module, you must call 'sodiumInit'.
-}
module Passetto.Crypto
  ( EncryptedKey
  , EncryptedKeyWithSalt
  , Password (..)
  , passwordFromText
  , encryptMasterKey
  , decryptMasterKey
  , encryptKey
  , decryptKey
  , encrypt
  , decrypt
  , publicToPem
  , Saltine.IsEncoding(..)

  , sodiumInit
  ) where

import Universum hiding (Key)

import Crypto.Error (throwCryptoError)
import qualified Crypto.KDF.Argon2 as Argon2
import qualified Crypto.PubKey.Curve25519 as Curve25519
import Crypto.Random (getRandomBytes)
import Crypto.Saltine (sodiumInit)
import qualified Crypto.Saltine.Class as Saltine
import qualified Crypto.Saltine.Core.Box as Box
import qualified Crypto.Saltine.Core.SecretBox as SecretBox
import qualified Crypto.Saltine.Internal.ByteSizes as Sizes
import qualified Crypto.Store.X509 as Store.X509
import Data.ByteArray (ScrubbedBytes)
import qualified Data.ByteArray as BA
import qualified Data.PEM as Pem
import qualified Data.X509 as X509

import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as LBS (fromStrict, toStrict)

-- Utils, move elsewhere
instance B.Binary Box.SecretKey
instance B.Binary Box.PublicKey

encodeStrict :: B.Binary b => b -> ByteString
encodeStrict = LBS.toStrict . B.encode

decodeStrict :: B.Binary b => ByteString -> b
decodeStrict = B.decode . LBS.fromStrict


{- The current implementation tries to mostly use @saltine@ package,
but we probably want to switch to something else one day, since saltine
* Lacks some bindings
* Provides bidings not present in NaCl and thus not completely reliable
* Uses ByteString instead of safer Scrubbedbytes, and other concerns
-}


-- | Password used for encrypting keys.
newtype Password = Password ScrubbedBytes

-- This is computed with symmetric encryption.
data EncryptedKey = EncryptedKey
  { emkSecret :: ByteString
    -- ^ Encrypted secret.
  , emkNonce :: SecretBox.Nonce
    -- ^ Nonce used to encrypt the key.
  } deriving stock (Eq, Generic)


instance B.Binary SecretBox.Nonce
instance B.Binary EncryptedKey

type EncryptedKeyWithSalt = (EncryptedKey, ByteString)

-- | Make a password from the user-supplied textual data.
passwordFromText :: Text -> Password
passwordFromText = Password . BA.convert @ByteString . encodeUtf8

-- | Encrypt key.
encryptKeyInternal :: MonadIO m => SecretBox.Key -> ByteString -> m EncryptedKey
encryptKeyInternal encryptionKey keyBs = do
  nonce <- liftIO SecretBox.newNonce
  let encSecret = SecretBox.secretbox encryptionKey nonce keyBs
  return EncryptedKey{ emkSecret = encSecret, emkNonce = nonce }

-- | Encrypt master key.
encryptMasterKey :: MonadIO m => Password -> SecretBox.Key -> m EncryptedKeyWithSalt
encryptMasterKey password sBk = do
  salt <- liftIO $ getRandomBytes kdfSaltSize
  let encryptionKey = mkEncryptionKey salt password
  (, salt) <$> encryptKeyInternal encryptionKey (Saltine.encode sBk)

-- Salt used in libsodium's crypto_pwhash
kdfSaltSize :: Int
kdfSaltSize = 16

-- | Decrypt master key.
decryptMasterKey :: EncryptedKeyWithSalt -> Password -> Maybe (SecretBox.Key)
decryptMasterKey (EncryptedKey{..}, emkKdfSalt) password =
    SecretBox.secretboxOpen encryptionKey emkNonce emkSecret >>= Saltine.decode
  where
    encryptionKey = mkEncryptionKey emkKdfSalt password

-- Derives a key for encrypting master.
mkEncryptionKey :: ByteString -> Password -> SecretBox.Key
mkEncryptionKey salt (Password pw) = do
  let skBs = Argon2.hash keyDerivationOptions pw salt Sizes.secretBoxKey
           & throwCryptoError  -- fails only when parameters are incorrect
  Saltine.decode skBs ?: error "Illegal secret box key size"

keyDerivationOptions :: Argon2.Options
keyDerivationOptions = Argon2.defaultOptions
  { Argon2.variant = Argon2.Argon2id
  , Argon2.version = Argon2.Version13
    --- ^ These are defaults currently used in libsodium's crypto_pwhash
  }

-- | Encrypt a key for further saving in storage.
encryptKey :: MonadIO m => SecretBox.Key -> Box.Keypair -> m EncryptedKey
encryptKey mk k =
  liftIO $ encryptKeyInternal mk (encodeStrict k)

decryptKey :: SecretBox.Key -> EncryptedKey -> Maybe Box.Keypair
decryptKey mk EncryptedKey{..} = decodeStrict <$> SecretBox.secretboxOpen mk emkNonce emkSecret

-- | Encrypt a user message.
encrypt :: MonadIO m => Box.PublicKey -> ByteString -> m ByteString
encrypt k msg = liftIO $ Box.boxSeal k msg

-- | Decrypt a user message.
decrypt :: Box.Keypair -> ByteString -> Maybe ByteString
decrypt (kSecret, kPublic) enc = Box.boxSealOpen kPublic kSecret enc

-- | Dump a public key into X.509 certificate.
publicToPem :: Box.PublicKey -> LByteString
publicToPem pubKey =
  Pem.pemWriteLBS . Store.X509.pubKeyToPEM . X509.PubKeyX25519 $
  throwCryptoError . Curve25519.publicKey $
  Saltine.encode pubKey
