module Passetto.Client.Util
  ( throwLeft
  ) where

import Universum

-- | Hepler which turns explicit error into thrown imprecise exception.
throwLeft :: (MonadThrow m, Exception e) => Either e a -> m a
throwLeft = either throwM pure
