module Passetto.KeysContext
  ( KeysContext (..)
  , HasKeysContext (..)
  , mkKeysContext
  , KeysStorageUnusable (..)

  , getKey
  , getKeysNum
  , getRandomKey
  , getModelVersion
  ) where

import Universum hiding (keys)

import qualified Crypto.Saltine.Core.Box as Box
import Data.SemVer (Version)
import qualified Data.Vector as V
import System.Random (randomRIO)

import Passetto.Version

-- | Provides access to stored keys.
--
-- Invariant: storage has to be initialized with at least one
-- encryption key.
data KeysContext = KeysContext
  { storedKeys :: Vector Box.Keypair
  , modelVersion :: Version
  }

mkKeysContext :: MonadThrow m => V.Vector Box.Keypair -> m KeysContext
mkKeysContext keys
  | V.null keys = throwM NoEncryptionKeys
  | otherwise = pure $ KeysContext keys encryptedContentVersion

data KeysStorageUnusable
  = NoEncryptionKeys
  deriving stock (Show)
instance Exception KeysStorageUnusable where
  displayException NoEncryptionKeys = "No encryption keys in storage"


class Monad m => HasKeysContext m where
  getKeysContext :: m KeysContext

instance Monad m => HasKeysContext (ReaderT KeysContext m) where
  getKeysContext = ask

instance HasKeysContext m => HasKeysContext (MaybeT m) where
  getKeysContext = lift getKeysContext

instance HasKeysContext m => HasKeysContext (ExceptT e m) where
  getKeysContext = lift getKeysContext

getKey :: HasKeysContext m => Word -> m (Maybe Box.Keypair)
getKey i = do
  KeysContext{..} <- getKeysContext
  return $ storedKeys V.!? fromIntegral i

getKeysNum :: HasKeysContext m => m Word
getKeysNum = do
  KeysContext{..} <- getKeysContext
  return $ fromIntegral (V.length storedKeys)

getRandomKey :: (MonadIO m, HasKeysContext m) => m (Word, Box.Keypair)
getRandomKey = do
  KeysContext{..} <- getKeysContext
  i <- liftIO $ randomRIO (0, V.length storedKeys - 1)
  let key = (storedKeys V.!? i) ?: error "unexpectedly no key"
  return (fromIntegral i, key)

getModelVersion :: HasKeysContext m => m Version
getModelVersion = modelVersion <$> getKeysContext
