-- | Low-level client methods.
module Passetto.Web.Client
  ( clientMethods
  , eEncryptJson
  , eDecryptJson
  ) where

import Universum

import Data.Aeson (FromJSON, Result, ToJSON, fromJSON, toJSON)
import Servant.Client.Core (RunClient)
import Servant.Client.Generic (AsClientT, genericClient)

import Passetto.Web.Routes
import Passetto.Web.Types

clientMethods :: RunClient m => AppEndpoints (AsClientT m)
clientMethods = genericClient

-- | Wrapper over 'eEncrypt' which encrypts arbitrary JSON data.
--
-- Note that this is low-level operation and its direct use for business logic
-- is discouraged.
eEncryptJson
  :: (ToJSON a, FromJSON b, Functor m)
  => AppEndpoints (AsClientT m) -> a -> m (Result b)
eEncryptJson methods =
  fmap (fromJSON . value) . eEncryptObj methods . EncBody . toJSON

-- | Wrapper over 'eDecrypt' which encrypts to arbitrary JSON data.
eDecryptJson
  :: (ToJSON a, FromJSON b, Functor m)
  => AppEndpoints (AsClientT m) -> a -> m (Result b)
eDecryptJson methods =
  fmap (fromJSON . value) . eDecryptObj methods . EncBody . toJSON
