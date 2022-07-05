module Passetto.Client.Error
  ( PassettoClientError (..)
  , reportJsonFailure
  ) where

import Universum

import Control.Monad.Except (MonadError(..))
import qualified Data.Aeson as Aeson
import Servant.Client (ClientError)
import qualified Text.Show

-- | Represents all possible errors during encryption/decryption.
data PassettoClientError
  = UnexpectedResponse Text
    -- ^ Server behaves unexpectedly.
  | PClientError ClientError
    -- ^ Connection issues.

instance Show PassettoClientError where
  show = \case
    UnexpectedResponse err ->
      "Unexpected response from passetto encryption server: " <>
      toString err
    PClientError err ->
      "Error when making request to passetto encryption server: " <>
      displayException err

instance Exception PassettoClientError where

reportJsonFailure :: MonadError PassettoClientError m => Aeson.Result a -> m a
reportJsonFailure = \case
  Aeson.Error err -> throwError $ UnexpectedResponse (toText err)
  Aeson.Success x -> pure x
