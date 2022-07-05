{-# LANGUAGE DerivingStrategies #-}

-- | Context for client methods.
module Passetto.Client.Context
  ( -- * Context
    PassettoContext (..)
  , mkDefPassettoContext
  , mkDefPassettoContextTLS
  , mkDefPassettoContextTLSDev
  , HasPassettoContext (..)
  , mkPassettoContext
  , withPassettoCtx
  , mkPassettoContextBasic
  , noEmptyCalls
  ) where

import Universum

import Control.Monad.Except (MonadError(..))
import Network.Connection (TLSSettings(..))
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Client.TLS (mkManagerSettings, newTlsManagerWith)
import Servant.Client (ClientEnv, ClientM, mkClientEnv, runClientM)
import Servant.Client.Core (BaseUrl(..), Scheme(..))

import Passetto.Client.Bulk
import Passetto.Client.Error
import Passetto.Util
import Passetto.Web.Client

-- | Context required for calling passetto client methods.
data PassettoContext = PassettoContext
  { bulkEncryptAction :: BulkEncryptCall
    -- ^ Encryption action.
  , bulkDecryptAction :: BulkDecryptCall
    -- ^ Decryption action.
    -- You may consider updating these two methods to include
    -- statistics collection, batching, e.t.c.
  }

-- | Construct a simple basic context.
--
-- Once encryption or decryption is called, this performs a request
-- to the server using low-level handlers from "Passetto.Web.Client" module.
--
-- Use this only if you need to define your own modifiers to actions within
-- 'PassettoContext', otherwise see 'mkPassettoContext'.
mkPassettoContextBasic :: ClientEnv -> PassettoContext
mkPassettoContextBasic clientEnv =
  PassettoContext
    { bulkEncryptAction = bulkEncryptImpl clientEnv
    , bulkDecryptAction = bulkDecryptImpl clientEnv
    }

-- | Avoid calls to server if no data should be encrypted or decrypted.
--
-- This behaviour may be actually useful, for instance if user needs to
-- encrypt two optional values it may be convenient if we handle @both absent@
-- case for him.
noEmptyCalls :: PassettoContext -> PassettoContext
noEmptyCalls PassettoContext{..} =
  PassettoContext
  { bulkEncryptAction = \case
      [] -> pure []
      xs -> bulkEncryptAction xs
  , bulkDecryptAction = \case
      [] -> pure []
      xs -> bulkDecryptAction xs
  }

-- | Construct a context ready for use.
mkPassettoContext :: ClientEnv -> PassettoContext
mkPassettoContext = noEmptyCalls . mkPassettoContextBasic

-- | Construct context with default options, specifying only server coordinates.
--
-- This uses plain unencrypted HTTP, which makes sense in case you handle
-- encryption from outside of the app.
mkDefPassettoContext :: MonadIO m => String -> Word16 -> m PassettoContext
mkDefPassettoContext host port = do
  manager <- liftIO $ newManager defaultManagerSettings
  let url = BaseUrl
        { baseUrlHost = host
        , baseUrlScheme = Http
        , baseUrlPort = fromIntegral port
        , baseUrlPath = ""
        }
  return . mkPassettoContext $ mkClientEnv manager url

-- | Construct context with default options, specifying only server coordinates;
-- communication will happen via HTTPS.
mkDefPassettoContextTLS :: MonadIO m => String -> Word16 -> m PassettoContext
mkDefPassettoContextTLS = mkDefPassettoContextTLS' False

-- | Like 'mkDefPassettoContextTLS', but disables TLS certificate verification.
mkDefPassettoContextTLSDev :: MonadIO m => String -> Word16 -> m PassettoContext
mkDefPassettoContextTLSDev = mkDefPassettoContextTLS' True

mkDefPassettoContextTLS' :: MonadIO m => Bool -> String -> Word16 -> m PassettoContext
mkDefPassettoContextTLS' allowInsecure host port = do
  let tlsSettings = TLSSettingsSimple
        { settingDisableCertificateValidation = allowInsecure
        , settingDisableSession = False
        , settingUseServerName = False
        }
  manager <- newTlsManagerWith (mkManagerSettings tlsSettings Nothing)
  let url = BaseUrl
        { baseUrlHost = host
        , baseUrlScheme = Https
        , baseUrlPort = fromIntegral port
        , baseUrlPath = ""
        }
  return . mkPassettoContext $ mkClientEnv manager url

-- | Provides context for passetto client operations.
class Monad m => HasPassettoContext m where
  getPassettoContext :: m PassettoContext

instance Monad m => HasPassettoContext (ReaderT PassettoContext m) where
  getPassettoContext = ask

instance HasPassettoContext m => HasPassettoContext (ExceptT e m) where
  getPassettoContext = lift getPassettoContext

-- | Helper which brings passetto client context in the scope.
-- Mostly for demo purposes.
withPassettoCtx
  :: Monad m
  => m PassettoContext -> ReaderT PassettoContext m a -> m a
withPassettoCtx newCtx action = newCtx >>= runReaderT action

-- ** Basic context implementation

runCli :: ClientEnv -> ClientM a -> ExceptT PassettoClientError IO a
runCli clientEnv cli =
  ExceptT . fmap (first PClientError) $ runClientM cli clientEnv

-- What do we send on encryption in batched operations
type BatchMessage = [Text]

-- Note: best practices suggest that client requests some key and
-- encrypts data locally, but here we do not do this yet.
bulkEncryptImpl :: ClientEnv -> BulkEncryptCall
bulkEncryptImpl cliEnv = \vals -> do
  let items = map encodeJsonT vals
  resVal <- runCli cliEnv $ eEncryptJson @BatchMessage clientMethods items
  encs <- reportJsonFailure resVal
  return $ map EncryptedRaw encs

bulkDecryptImpl :: ClientEnv -> BulkDecryptCall
bulkDecryptImpl cliEnv = \encs -> do
  resVal <-
    runCli cliEnv $
    eDecryptJson @_ @BatchMessage clientMethods (map unEncryptedRaw encs)
  items <- reportJsonFailure resVal
  forM items $ either (throwError . UnexpectedResponse) pure . decodeJsonT
