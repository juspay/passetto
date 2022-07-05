{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-deriving-typeable #-}
{-# LANGUAGE TemplateHaskell #-}

-- The module is intended to facilitate development: separate long-compiling TH out
module Passetto.DbType
  ( openDb, closeDb, checkpoint
  , Db
  , viewMaster, viewKeys, putMaster, addKeys
  ) where

import Universum hiding (keys)

import Data.Acid (AcidState, Query, Update, makeAcidic, query, update, openLocalState, closeAcidState, createCheckpoint)
import Data.SafeCopy (base, deriveSafeCopy)

import Crypto.Saltine.Core.SecretBox (Nonce)
import Passetto.Crypto (EncryptedKey, EncryptedKeyWithSalt)

data DbTy = DbTy (Maybe EncryptedKeyWithSalt) [[EncryptedKey]]

type Db = AcidState DbTy

emptyDb :: DbTy
emptyDb = DbTy Nothing []

-- NOTE: Since the backend will perhaps be swapped,
--         we put all safecopy orphans here
$(deriveSafeCopy 0 'base ''Nonce)
$(deriveSafeCopy 0 'base ''EncryptedKey)
$(deriveSafeCopy 0 'base ''DbTy)

viewMaster_ :: Query DbTy (Maybe EncryptedKeyWithSalt)
viewMaster_ = (\(DbTy em _) -> em) <$> ask

viewKeys_ :: Query DbTy [[EncryptedKey]]
viewKeys_ = (\(DbTy _ keys) -> keys) <$> ask

putMaster_ :: EncryptedKeyWithSalt -> Update DbTy ()
putMaster_ em = get >>= (\(DbTy _ keys) -> put $ DbTy (Just em) keys)

addKeys_ :: [EncryptedKey] -> Update DbTy ()
addKeys_ keys = get >>= (\(DbTy em ekeys) -> put $ DbTy em $ keys:ekeys)

$(makeAcidic ''DbTy ['viewMaster_, 'viewKeys_, 'putMaster_, 'addKeys_])

viewMaster :: Db -> IO (Maybe EncryptedKeyWithSalt)
viewMaster = flip query ViewMaster_

viewKeys :: Db -> IO [EncryptedKey]
viewKeys db = (concat . reverse) <$> query db ViewKeys_ -- FIXME?: Do we need manual opt here?

putMaster :: Db -> EncryptedKeyWithSalt -> IO ()
putMaster db = update db . PutMaster_

addKeys :: Db -> [EncryptedKey] -> IO ()
addKeys db = update db . AddKeys_

openDb :: IO Db
openDb = openLocalState emptyDb

closeDb :: Db -> IO ()
closeDb = closeAcidState

checkpoint :: Db -> IO ()
checkpoint = createCheckpoint
