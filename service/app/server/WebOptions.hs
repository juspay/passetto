{-# OPTIONS_GHC -Wno-orphans #-}

module WebOptions
  ( Options (..)
  , getOptions
  , EnvParseException (..)
  ) where

import Universum

import System.Environment (lookupEnv)

import Passetto.Crypto
import Passetto.Web.Server

data EnvParseException
  = OptionMissing String
  | ParseFailed Text
  deriving stock (Show)

instance Exception EnvParseException where
  displayException = \case
    OptionMissing name -> "Missing env variable: " <> name
    ParseFailed err -> "Env variable parsing failed: " <> toString err

data Options = Options
  { password :: Password
  , port :: Word16
  , schemeOptions :: SchemeConfig
  } deriving stock Generic

getEnv :: String -> IO Text
getEnv key =
  lookupEnv key >>= maybe (throwM $ OptionMissing key) (pure . fromString)

parseEnv :: String -> (String -> Either Text a) -> IO a
parseEnv val parser = either (throwM . ParseFailed) pure $ parser val

getPassword :: IO Password
getPassword = passwordFromText <$> getEnv "MASTER_PASSWORD"

getPort :: IO Word16
getPort = do
  txt <- fromMaybe "8012" <$> lookupEnv "PORT"
  parseEnv txt $ maybeToRight ("Failed to parse port: " <> toText txt) . readMaybe

getTlsConfig :: IO TLSConfig
getTlsConfig = do
  mCertPath <- lookupEnv tlsCertVar
  mKeyPath <- lookupEnv tlsKeyVar
  case (mCertPath, mKeyPath) of
    (Just certPath, Just keyPath) ->
      pure TLSConfig{..}
    (Nothing, Nothing) -> pure TLSConfig
      { certPath = "./cert/localhost.crt"
      , keyPath = "./cert/localhost.key"
      }
    (Just{}, Nothing) ->
      throwM $ OptionMissing tlsKeyVar
    (Nothing, Just{}) ->
      throwM $ OptionMissing tlsCertVar
  where
    tlsKeyVar = "TLS_KEY"
    tlsCertVar = "TLS_CERT"

getScheme :: IO SchemeConfig
getScheme = 
  lookupEnv "USE_TLS" >>= \case
    Nothing -> pure HttpScheme 
    Just _ -> HttpsScheme <$> getTlsConfig
    
getOptions :: IO Options
getOptions = do
  password <- getPassword
  port <- getPort
  schemeOptions <- getScheme
  return Options{..}
