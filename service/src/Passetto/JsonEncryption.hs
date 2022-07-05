module Passetto.JsonEncryption
  ( DecryptError (..)
  , decryptToServerError

  , encryptPayload
  , decryptPayload
  , encryptJsonPayload
  , decryptJsonPayload
  ) where

import Universum

import Control.Monad.Except (throwError)
import Data.Aeson (Value)
import qualified Data.Aeson as A
import qualified Data.SemVer as V
import qualified Data.Text as T
import Servant (ServerError(..), err410, err422)

import qualified Passetto.Crypto as Crypto
import Passetto.KeysContext
import Passetto.Util (decodeBase64, encodeBase64)

data DecryptError
  = InvalidEncryptedData
  | UnsupportedModelVersion [V.Version] V.Version
  deriving stock (Show, Eq)

decryptToServerError :: DecryptError -> ServerError
decryptToServerError = \case
  InvalidEncryptedData ->
    err422{ errBody = "Encrypted data is invalid." }
  UnsupportedModelVersion supported actual ->
    err410{ errBody = encodeUtf8 $
      "Encryption model version " <> V.toText actual <> " is not supported, \
      \allowed versions: [" <> mconcat (intersperse ", " $ V.toText <$> supported) <> "]"
    }

-- | Encrypts text.
encryptPayload :: (MonadIO m, HasKeysContext m) => Text -> m Text
encryptPayload msg = do
  ver <- getModelVersion
  (idx, keypair) <- getRandomKey
  encrypted <- Crypto.encrypt (snd keypair) (encodeUtf8 msg)
  return . toText . mconcat $ intersperse "|"
    [ V.toLazyText ver
    , toLText $ show @Text idx
    , toLText $ encodeBase64 encrypted
    ]

-- | The inverse to 'encryptPayload'.
decryptPayload :: (HasKeysContext m) => Text -> ExceptT DecryptError m Text
decryptPayload encrypted =
  maybe (throwError InvalidEncryptedData) pure <=< runMaybeT $ do
    -- if caller does not provide "|" at all, probably he tried to construct
    -- request manually, let's fail with decryption error in such case
    verTxt : pieces <- pure $ T.splitOn "|" encrypted
    supportedVer <- getModelVersion
    Right ver <- pure $ V.fromText verTxt
    unless (ver == supportedVer) $
      throwError (UnsupportedModelVersion [supportedVer] ver)

    [idxTxt, cipher64] <- pure pieces
    Just idx <- pure $ readMaybe (toString idxTxt)
    Just key <- getKey idx

    Just cipher <- pure $ decodeBase64 cipher64
    Just origin <- pure $ Crypto.decrypt key cipher
    Right originTxt <- pure $ decodeUtf8' origin
    return originTxt

-- | Encrypts leaves of the given JSON data in the same way as 'encryptPayload'
-- does.
encryptJsonPayload :: (MonadIO m, HasKeysContext m) => Value -> m Value
encryptJsonPayload = \case
  A.Array vs -> A.Array <$> mapM encryptJsonPayload vs
  A.Object vs -> A.Object <$> mapM encryptJsonPayload vs
  A.String s -> A.String <$> encryptPayload ("S" <> s)
  A.Bool b -> A.String <$> encryptPayload (if b then "T" else "F")
  A.Number n -> A.String <$> encryptPayload ("N" <> show n)
  A.Null -> A.String <$> encryptPayload "0"

-- | The inverse to 'decryptPayload'.
decryptJsonPayload :: (HasKeysContext m) => Value -> ExceptT DecryptError m Value
decryptJsonPayload = \case
  A.Array vs -> A.Array <$> mapM decryptJsonPayload vs
  A.Object vs -> A.Object <$> mapM decryptJsonPayload vs
  A.Bool _ -> throwError InvalidEncryptedData
  A.Number _ -> throwError InvalidEncryptedData
  A.Null -> throwError InvalidEncryptedData
  A.String encrypted -> do
    tagged <- decryptPayload encrypted
    case T.uncons tagged of
      Just ('S', txt) ->
        pure $ A.String txt
      Just ('N', readMaybe . toString -> Just txt) ->
        pure $ A.Number txt
      Just ('F', "") ->
        pure $ A.Bool False
      Just ('T', "") ->
        pure $ A.Bool True
      Just ('0', "") ->
        pure $ A.Null
      _ ->
        throwError InvalidEncryptedData
