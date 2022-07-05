module Passetto.Version
  ( encryptedContentVersion
  ) where

import Universum

import qualified Data.SemVer as V

-- | Declares actual version of encrypted data format.
--
-- When format of some of the server's @/encrypt@ endpoint changes,
-- this constant should be bumped as well.
encryptedContentVersion :: V.Version
encryptedContentVersion = V.initial & V.minor .~ 1
