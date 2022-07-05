module KeysDb (main) where

import Universum
import System.Environment (getArgs)

import Passetto.Crypto (Password(..), encode)
import Passetto.Db (openDb, appendDb, readAndDecryptDb)

main :: IO ()
main = do
    db <- openDb
    let
      doit [p, nstr]
        | Just n <- readMaybe nstr = appendDb (mkPwd p) n db >>= either quit (const $ putTextLn "Ok")
      doit [p] = readAndDecryptDb (mkPwd p) db >>= either quit (mapM_ (print . bimap encode encode) . toList)
      doit _ = putStrLn @String logo
    getArgs >>= doit
  where
    mkPwd = Password . fromString
    quit = die . displayException
    logo = concat
      [ "usage: KeyDb <password> [nKeys]\n\n"
      , "When invoked with nKeys parameter appends `nKeys` fresh keys to the database "
      , "(if the db isn't initialized, generates new master key from the password, otherwise checks the password against existing master key). "
      , "Otherwise decrypts and dumps the database to stdout (DEV ONLY!!!)."
      ]
