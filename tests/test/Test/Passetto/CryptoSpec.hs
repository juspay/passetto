module Test.Passetto.CryptoSpec
  ( spec
  ) where

import Universum

import qualified Crypto.Saltine.Core.Box as Box
import qualified Crypto.Saltine.Core.SecretBox as SecretBox
import qualified Data.ByteArray as BA
import Hedgehog ((===))
import qualified Hedgehog as H
import qualified Hedgehog.Gen as HG
import qualified Hedgehog.Range as HR
import Test.Hspec (Spec, describe, it, runIO)
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

import Passetto.Crypto

spec :: Spec
spec = do
  runIO sodiumInit

  describe "Payload encryption" $ do
    it "Roundtrip" . hedgehog $ do
      key <- liftIO Box.newKeypair
      payload <- H.forAll $ HG.bytes (HR.exponential 1 4096)
      enc <- encrypt (snd key) payload
      let res = decrypt key enc
      res === Just payload

  describe "Key encryption" $ do
    it "Roundtrip" . hedgehog $ do
      master <- liftIO SecretBox.newKey
      key <- liftIO Box.newKeypair
      enc <- encryptKey master key
      let res = decryptKey master enc
      fmap H.Opaque res === Just (H.Opaque key)

  describe "Master encryption" $ do
    modifyMaxSuccess (max 1 . (`div` 10)) . it "Roundtrip" . hedgehog $ do
      password <- H.forAllWith (const "<password>") genPassword
      master <- liftIO SecretBox.newKey
      encMaster <- encryptMasterKey password master
      let res = decryptMasterKey encMaster password
      fmap H.Opaque res === Just (H.Opaque master)

genPassword :: H.Gen Password
genPassword = Password . BA.convert <$> HG.bytes (HR.constant 1 100)
