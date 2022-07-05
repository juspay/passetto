module Passetto.Web.Types
  ( EncBody (..)
  , ServiceStatus (..)
  ) where

import Universum

import Data.Aeson.TH (deriveJSON)

import Passetto.Util

-- | Request/response body for @encrypt@ and @decrypt@ endpoints.
data EncBody a = EncBody
  { value :: a
  }
deriveJSON aesonOptions ''EncBody

-- | Information about the current status of the service.
data ServiceStatus = ServiceStatus
  { keysNumber         :: Word
  }
deriveJSON aesonOptions ''ServiceStatus
