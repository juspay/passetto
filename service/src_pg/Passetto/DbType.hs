{-# OPTIONS_GHC -Wno-orphans #-}

module Passetto.DbType
  ( openDb, closeDb, checkpoint
  , Db
  , viewMaster, viewKeys, putMaster, addKeys
  ) where

import Universum
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as L
import System.Environment (getEnv)

import Database.PostgreSQL.Simple (Connection, connectPostgreSQL, query, execute, executeMany, withTransaction, close, Binary(..), Only(..))
import Passetto.Crypto (EncryptedKey, EncryptedKeyWithSalt)

type Db = Connection

tof :: B.Binary a => a -> Only (Binary ByteString)
tof = Only . Binary . L.toStrict . B.encode
fromf :: B.Binary a => Only (Binary ByteString) -> a
fromf = B.decode . L.fromStrict . fromBinary . fromOnly

viewMaster :: Db -> IO (Maybe EncryptedKeyWithSalt)
viewMaster c = do
  ms <- query c "SELECT (key) FROM \"Passetto\".\"Master\"" ()
  case ms of
    [] -> pure Nothing
    [m] -> pure $ Just $ fromf m
    -- FIXME! Shall throw
    _   -> pure Nothing

viewKeys :: Db -> IO [EncryptedKey]
viewKeys c = do
  eks <- query c "SELECT (encryptedKeyPair) FROM \"Passetto\".\"Keys\" ORDER BY id" ()
  pure $ map fromf eks

putMaster :: Db -> EncryptedKeyWithSalt -> IO ()
putMaster c em = withTransaction c $ do
  _ <- execute c "DELETE FROM \"Passetto\".\"Master\"" ()
  void $ execute c "INSERT INTO \"Passetto\".\"Master\"(key) values (?)" (tof em)

addKeys :: Db -> [EncryptedKey] -> IO ()
addKeys c eks =
  void $ executeMany c "INSERT INTO \"Passetto\".\"Keys\"(encryptedKeyPair) values(?)" (map tof eks)

openDb :: IO Db
openDb = connectPostgreSQL =<< fromString <$> getEnv "PASSETTO_PG_BACKEND_CONN_STRING"

closeDb :: Db -> IO ()
closeDb = close

checkpoint :: Db -> IO ()
checkpoint _ = pure ()
