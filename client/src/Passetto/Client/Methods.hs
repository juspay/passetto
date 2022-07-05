-- | Functions which user should eventually use.
module Passetto.Client.Methods
  ( cliEncryptOne
  , cliDecryptOne
  , cliEncrypt
  , cliDecrypt
  ) where

import Universum

import Data.Aeson (FromJSON, ToJSON)

import Passetto.Client.Context
import Passetto.Client.EncryptedItem
import Passetto.Client.Error

-- | Encrypt a value or a complex structure.
--
-- This performs not more than one call to server, so try to avoid using multiple
-- subsequent invocations of this method in favor of passing complex structures
-- through it.
cliEncrypt
  :: forall e m.
     (MonadIO m, HasPassettoContext m, EncryptedItem e)
  => Unencrypted e -> m (Either PassettoClientError e)
cliEncrypt val = do
  PassettoContext{..} <- getPassettoContext
  liftIO . runExceptT . runBulkM bulkEncryptAction $ encryptItem val

-- | Decrypt a value similarly to 'encrypt' method.
cliDecrypt
  :: forall e m.
     (MonadIO m, HasPassettoContext m, EncryptedItem e)
  => e -> m (Either PassettoClientError $ Unencrypted e)
cliDecrypt enc = do
  PassettoContext{..} <- getPassettoContext
  liftIO . runExceptT . runBulkM bulkDecryptAction $ decryptItem enc

-- | Encrypt a single value.
--
-- This is a type-restricted version of 'encrypt'.
cliEncryptOne
  :: forall a m.
     (MonadIO m, HasPassettoContext m, ToJSON a, FromJSON a)
  => a -> m (Either PassettoClientError $ Encrypted a)
cliEncryptOne = cliEncrypt

-- | Decrypt a single value.
--
-- This is a type-restricted version of 'decrypt'.
cliDecryptOne
  :: forall a m.
     (MonadIO m, HasPassettoContext m, ToJSON a, FromJSON a)
  => Encrypted a -> m (Either PassettoClientError a)
cliDecryptOne = cliDecrypt
