{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Client for working with passetto server.
module Passetto.Client
  ( -- * Context
    PassettoContext (..)
  , mkDefPassettoContext
  , mkDefPassettoContextTLS
  , mkDefPassettoContextTLSDev
  , HasPassettoContext (..)
  , mkPassettoContext
  , withPassettoCtx

    -- * Encryption methods
  , Encrypted
  , EncryptedBase64 (..)
  , EncryptedItem (..)
  , cliEncrypt
  , cliDecrypt
  , cliEncryptOne
  , cliDecryptOne

    -- * Errors
  , PassettoClientError (..)

    -- * 'EncryptedGeneral' implementations
  , genericEncryptItem
  , genericDecryptItem

    -- * Helpers
  , throwLeft
  ) where

import Passetto.Client.Context
import Passetto.Client.EncryptedItem
import Passetto.Client.Error
import Passetto.Client.Methods
import Passetto.Client.Util
