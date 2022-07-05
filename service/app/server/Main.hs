module Main (main) where

import Universum

import Control.Monad.Catch (handleAll)

import Passetto.Crypto
import Passetto.Db
import Passetto.KeysContext
import Passetto.Web.Server

import WebOptions

withExceptionsHandler :: IO () -> IO ()
withExceptionsHandler = handleAll $ die . ("exited: " <>) . displayException

withKeysStorage :: Password -> (KeysContext -> IO a) -> IO a
withKeysStorage password action =
  bracket openDb closeDb $ \acidState -> do
    keysVec <-
      readAndDecryptDb password acidState
      >>= either throwM pure
    keysCtx <- mkKeysContext keysVec
    action keysCtx

main :: IO ()
main = withExceptionsHandler $ do
  opts <- getOptions
  withKeysStorage (password opts) $ \keysCtx -> do
    putTextLn "Starting server..."
    runServer keysCtx (schemeOptions opts) (port opts)
