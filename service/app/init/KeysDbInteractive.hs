module KeysDbInteractive (main) where

import Universum
import System.Environment (getArgs)

import Passetto.Crypto (encode)
import Passetto.Db (openDb, appendDbInteractive, readAndDecryptDbInteractive, changePasswordInteractive)

main :: IO ()
main = do
    db <- openDb
    let
      doit ["addkeys", nstr]
        | Just n <- readMaybe nstr = appendDbInteractive n db >>= print
      doit ["dump"] = readAndDecryptDbInteractive db >>= either print (mapM_ (print . bimap encode encode) . toList)
      doit ["newpass"] = changePasswordInteractive db >>= print
      doit _ = putStrLn @String logo
    getArgs >>= doit
  where
    logo = concat
      [ "usage: KeyDb addkeys nKeys | dump | newpass\n"
      , "* `addkeys` appends `nKeys` fresh keys to the database "
      , "(if the db isn't initialized, generates new master key from the password, otherwise checks the password against existing master key).\n"
      , "* `newpass` allows the user to change the  database password (or set a fresh one if the DB is empty)\n"
      , "* `dump` dumps the database to stdout (DEV ONLY!!!)."
      ]
