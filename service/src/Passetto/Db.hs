module Passetto.Db
  ( openDb, closeDb
  , appendDb, appendDbInteractive
  , changePassword, changePasswordInteractive
  , readAndDecryptDb, readAndDecryptDbInteractive
  , DbErr(..), Db
  ) where

import Data.Vector (fromList)
import Universum hiding (fromList)

import qualified Crypto.Saltine.Core.Box as Box
import qualified Crypto.Saltine.Core.SecretBox as SecretBox
import Passetto.Crypto
  (EncryptedKeyWithSalt, Password, decryptKey, decryptMasterKey, encryptKey,
  encryptMasterKey, sodiumInit)
import Passetto.HandlePassword (getPassword, tryPasswordOnMaster)
import Passetto.DbType

data DbErr =
    DbWrongPassword
  | DbNotInitialized
  | DbCorrupted
  deriving stock (Generic, Show)

instance Exception DbErr where
  displayException = \case
    DbWrongPassword -> "Invalid password"
    DbNotInitialized -> "Database has not been initialized"
    DbCorrupted -> "Database corrupted"

type DbR e a = IO (Either e a)

type DbOp e a = Db -> DbR e a

data OpErr =
    OpFailConfirmedInput
  | OpDbErr DbErr
  deriving stock (Generic, Show)

-- Utils
withMaster :: IO a -> (EncryptedKeyWithSalt -> IO a) -> Db -> IO a
withMaster fn fe db = sodiumInit >> viewMaster db >>= maybe fn fe

storeMaster :: Db -> SecretBox.Key -> Password -> IO ()
storeMaster db m pnew =
  encryptMasterKey pnew m >>= putMaster db

putNewPass :: Password -> Db -> IO SecretBox.Key
putNewPass p db = do
  m <- SecretBox.newKey
  storeMaster db m p
  checkpoint db
  pure m

vPutNewPass :: Db -> Password -> DbR e ()
vPutNewPass db p = Right <$> (void $ putNewPass p db)

err :: e -> DbR e a
err = pure . Left

onEncMaster :: Password -> (SecretBox.Key -> DbR DbErr a) -> EncryptedKeyWithSalt -> DbR DbErr a
onEncMaster p f em =
  maybe (err DbWrongPassword) f $ decryptMasterKey em p

onEncMasterInteractive :: (SecretBox.Key -> DbR OpErr a) -> EncryptedKeyWithSalt -> DbR OpErr a
onEncMasterInteractive f em =
  tryPasswordOnMaster em >>=
      maybe (err $ OpDbErr DbWrongPassword) f

ok :: DbR e ()
ok = pure $ Right ()

toOpErr :: DbR DbErr a -> DbR OpErr a
toOpErr e = first OpDbErr <$> e

withGetPassword :: (Password -> DbR OpErr a) -> DbR OpErr a
withGetPassword f = getPassword >>= maybe (pure $ Left OpFailConfirmedInput) f

-- Exported functions
appendDb :: Password -> Int -> DbOp DbErr ()
appendDb p n db = withMaster ini (onEncMaster p createAndAddKeys) db
  where
    createAndAddKeys m = do
      sequence (replicate n $ Box.newKeypair >>= encryptKey m)
         >>= addKeys db
      ok
    ini = putNewPass p db >>= createAndAddKeys

appendDbInteractive :: Int -> DbOp OpErr ()
appendDbInteractive n db = withGetPassword (\p -> toOpErr $ appendDb p n db)

changePassword :: Password -> Password -> DbOp DbErr ()
changePassword oldp newp db = withMaster (vPutNewPass db newp) (onEncMaster oldp $ \m -> storeMaster db m newp >> ok) db

changePasswordInteractive :: DbOp OpErr ()
changePasswordInteractive db = withMaster (withGetPassword $ vPutNewPass db) (onEncMasterInteractive getNew) db
  where
    getNew m = withGetPassword $ fmap Right . storeMaster db m

readAndDecryptDb :: Password -> DbOp DbErr (Vector Box.Keypair)
readAndDecryptDb p = readAndDecryptDb' err (onEncMaster p)

readAndDecryptDbInteractive :: DbOp OpErr (Vector Box.Keypair)
readAndDecryptDbInteractive db = readAndDecryptDb' (toOpErr . err) (onEncMasterInteractive . (toOpErr .)) db

-- Another util
type R e = DbR e (Vector Box.Keypair)

readAndDecryptDb' ::
     (DbErr -> R e)
  -> ((SecretBox.Key -> DbR DbErr (Vector Box.Keypair)) -> EncryptedKeyWithSalt -> R e)
  -> Db
  -> R e
readAndDecryptDb' e checkp db = withMaster (e DbNotInitialized) (checkp getKeys) db
  where
    getKeys m = do
      ks <- viewKeys db
      let
        kpairs = catMaybes $ map (decryptKey m) ks
        n = length kpairs
      pure $ if n /= length ks
       then Left DbCorrupted
       else Right $ fromList kpairs
