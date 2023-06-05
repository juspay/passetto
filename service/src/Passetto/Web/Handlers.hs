module Passetto.Web.Handlers
  ( appHandlers
  ) where

import Universum

import qualified Crypto.Saltine.Class as Saltine
import qualified Crypto.Saltine.Core.Box as Box
import Servant (ServerError(..), err404)
import Servant.Server.Generic (AsServerT)

import Passetto.Crypto (publicToPem)
import Passetto.JsonEncryption
import Passetto.KeysContext
import Passetto.Util (encodeBase64)
import Passetto.Web.Routes
import Passetto.Web.Types

appHandlers
  :: (MonadIO m, MonadThrow m, HasKeysContext m)
  => AppEndpoints (AsServerT m)
appHandlers = AppEndpoints
  { eEncrypt = \(EncBody x) ->
      EncBody <$> encryptPayload x

  , eEncryptObj = \(EncBody x) ->
      EncBody <$> encryptJsonPayload x

  , eDecrypt = \(EncBody x) -> do
      eres <- runExceptT (decryptPayload x)
      res <- either (throwM . decryptToServerError) pure eres
      return (EncBody res)

  , eDecryptObj = \(EncBody x) -> do
      eres <- runExceptT (decryptJsonPayload x)
      res <- either (throwM . decryptToServerError) pure eres
      return (EncBody res)

  , eGetKey = \idx -> do
      keyPair <- getKeyOrThrow idx
      return $ encodeBase64 (Saltine.encode $ Box.publicKey keyPair)

  , eGetKeyCert = \idx -> do
      keyPair <- getKeyOrThrow idx
      return $ decodeUtf8 $ publicToPem $ Box.publicKey keyPair

  , eStatus = getServiceStatus
  }

getKeyOrThrow :: (MonadThrow m, HasKeysContext m) => Word -> m Box.Keypair
getKeyOrThrow idx = do
  mkey <- getKey idx
  maybe (throwM err404{ errBody = "No key found" }) pure mkey

getServiceStatus :: HasKeysContext m => m ServiceStatus
getServiceStatus = do
  keysNumber <- getKeysNum
  return ServiceStatus{..}
