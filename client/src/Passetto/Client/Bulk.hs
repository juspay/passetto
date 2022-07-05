{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Provides helpers to implement bulk operations.
module Passetto.Client.Bulk
  ( BulkCall
  , BulkM
  , bulkSend
  , bulkParsing
  , runBulkM

  , EncryptedRaw (..)
  , BulkEncryptCall
  , EncryptM
  , BulkDecryptCall
  , DecryptM
  ) where

import Universum

import Control.Monad.Except (MonadError(..))
import qualified Data.Aeson as Aeson

import Passetto.Client.Error

-- * Bulk applicative

-- | Bulk encrypt/decrypt method signature.
type BulkCall i o = [i] -> ExceptT PassettoClientError IO [o]

-- | Applicative which constructs bulk call (to server) from individual calls.
data BulkM i o a = BulkM
  { bInput :: Endo [i]
    -- ^ Hand crafted DList with input
  , bOutputReader :: StateT [o] (ExceptT PassettoClientError IO) a
    -- ^ Consumes and parses results
  } deriving stock (Functor)

instance Applicative (BulkM i o) where
  pure x = BulkM{ bInput = mempty, bOutputReader = pure x }
  f <*> a =
    BulkM
    { bInput = bInput f <> bInput a
    , bOutputReader = bOutputReader f <*> bOutputReader a
    }

-- | Submit one item as part of bulk operation.
bulkSend :: i -> BulkM i o o
bulkSend inp = BulkM
  { bInput = Endo (inp :)
  , bOutputReader = StateT $ \case
      [] ->
        throwError $ UnexpectedResponse
        "Server returned less items than were passed on encryption"
      o : os -> pure (o, os)
  }

-- | Apply parser to the returned value.
bulkParsing :: BulkM i o a -> (a -> Either Text b) -> BulkM i o b
bulkParsing action parser =
  action
  { bOutputReader = bOutputReader action >>= \a -> case parser a of
      Left err -> throwError $ UnexpectedResponse err
      Right x -> pure x
  }

-- | Invoke the entire bulk operation.
runBulkM :: BulkCall i o -> BulkM i o a -> ExceptT PassettoClientError IO a
runBulkM call BulkM{..} = do
  output <- call (appEndo bInput [])
  (res, leftover) <- runStateT bOutputReader output
  unless (null leftover) $
    throwError $ UnexpectedResponse
    "Server returned more items than were passed on encryption"
  return res

-- | Some encrypted entry.
newtype EncryptedRaw = EncryptedRaw { unEncryptedRaw :: Text }

-- | Signature of "bulk encrypt" method.
type BulkEncryptCall = BulkCall Aeson.Value EncryptedRaw

-- | Provides bulk encryption context.
type EncryptM = BulkM Aeson.Value EncryptedRaw

-- | Signature of "bulk decrypt" method.
type BulkDecryptCall = BulkCall EncryptedRaw Aeson.Value

-- | Provides bulk decryption context.
type DecryptM = BulkM EncryptedRaw Aeson.Value
