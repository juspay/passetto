module Passetto.Web.Routes
  ( AppEndpoints (..)
  , AppAPI
  ) where

import Universum

import Data.Aeson (Value)
import Servant.API.Generic (type (:-), ToServantApi)
import Servant.API ((:>), JSON, Get, Capture, PlainText, ReqBody, Post)

import Passetto.Web.Types


type AppAPI = ToServantApi AppEndpoints

data AppEndpoints route = AppEndpoints
  { -- | Returns meta information about the current state of the server.
    eStatus
      :: route
      :- "status"
      :> Get '[JSON] ServiceStatus

    -- | Encrypts given text. Returned value will be base64-encoded.
    --
    -- This implements a corner case of 'eEncryptObj'.
  , eEncrypt
      :: route
      :- "encrypt"
      :> ReqBody '[JSON] (EncBody Text)
      :> Post '[JSON] (EncBody Text)

    -- | Encrypts a JSON object recursively, returned value will have the same
    -- structure as the original JSON but with atomic values replaced with
    -- ciphertext.
  , eEncryptObj
      :: route
      :- "encrypt"
      :> "obj"
      :> ReqBody '[JSON] (EncBody Value)
      :> Post '[JSON] (EncBody Value)

    -- | The reverse to 'eEncrypt'.
  , eDecrypt
      :: route
      :- "decrypt"
      :> ReqBody '[JSON] (EncBody Text)
      :> Post '[JSON] (EncBody Text)

    -- | The reverse to 'eEncryptObj'.
  , eDecryptObj
      :: route
      :- "decrypt"
      :> "obj"
      :> ReqBody '[JSON] (EncBody Value)
      :> Post '[JSON] (EncBody Value)

    -- | Returns an encryption key in base64 format.
  , eGetKey
      :: route
      :- "keys"
      :> Capture "id" Word
      :> Get '[PlainText] Text

    -- | Returns an encryption key in PEM file format.
  , eGetKeyCert
      :: route
      :- "keys"
      :> Capture "id" Word
      :> "cert"
      :> Get '[PlainText] LText

  } deriving stock (Generic)
