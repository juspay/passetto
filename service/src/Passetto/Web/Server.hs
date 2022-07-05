module Passetto.Web.Server
  ( TLSConfig (..)
  , SchemeConfig (..)
  , server
  , runServer
  ) where

import Universum

import qualified Control.Exception.Safe as E
import Control.Monad.Except (throwError)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (Settings, runSettings, defaultSettings, setPort, setServerName)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import Servant (Handler, ServerError(..), err500)
import Servant.Server.Generic (genericServeT)

import Passetto.KeysContext
import Passetto.Web.Handlers

data TLSConfig = TLSConfig
  { certPath :: FilePath
  , keyPath  :: FilePath
  }

data SchemeConfig
  = HttpScheme
  | HttpsScheme TLSConfig

runScheme :: SchemeConfig -> Settings -> Application -> IO ()
runScheme = \case
  HttpScheme -> runSettings
  HttpsScheme tls -> runTLS $ tlsSettings (certPath tls) (keyPath tls)

runServer :: KeysContext -> SchemeConfig -> Word16 -> IO a
runServer keysCtx serverScheme port =
  runScheme serverScheme settings (server keysCtx) $> error "Server stopped"
  where
    settings =
      defaultSettings
        & setPort (fromIntegral port)
        & setServerName "Encryption service"

server :: KeysContext -> Application
server keysCtx = genericServeT hst appHandlers
  where
    hst :: ReaderT KeysContext IO a -> Handler a
    hst handler =
      liftIO (runReaderT handler keysCtx) `E.catches`
        [ E.Handler $ \(e :: ServerError) ->
            throwError e
        , E.Handler $ \(e :: SomeException) ->
            throwError $ err500{ errBody = "Internal error occurred: " <> show e }
        ]
