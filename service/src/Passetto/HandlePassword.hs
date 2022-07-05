module Passetto.HandlePassword (getPassword, tryPasswordOnMaster) where

import Universum

import qualified Crypto.Saltine.Core.SecretBox as SecretBox (Key)
import qualified System.Console.Haskeline as HL (defaultSettings, getPassword, runInputT)

import Passetto.Crypto (EncryptedKeyWithSalt, decryptMasterKey, Password (..))


getpwd :: String -> IO (Maybe String)
getpwd s = HL.runInputT HL.defaultSettings (HL.getPassword (Just '*') s)

mkPassword :: String -> Password
mkPassword = Password . fromString

getPassword :: IO (Maybe Password)
getPassword = do
  mbp1 <- getpwd "Please, enter new password.\n"
  mbp2 <- getpwd "Please, confirm the password.\n"
  pure $ if isJust mbp1 && mbp1 == mbp2 then mkPassword <$> mbp1 else Nothing

-- Number of attempts is hardcoded ATM
numOfAttempts :: Int
numOfAttempts = 5

tryPasswordOnMaster :: EncryptedKeyWithSalt -> IO (Maybe SecretBox.Key)
tryPasswordOnMaster em = do
    putStr @String "Please, enter password to access the DB, " >> doit numOfAttempts
  where
    doit 0 = pure Nothing
    doit n =
      let onFail = putStr @String "Failed, " >> doit (n - 1)
      in getpwd (show n ++ " attempts left:\n") >>=
           maybe onFail (maybe onFail (pure . Just) . decryptMasterKey em . mkPassword)
