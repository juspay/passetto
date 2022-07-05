module Test.Passetto.PayloadEncryptionSpec
  ( spec
  ) where

import Universum

import Hedgehog ((===))
import qualified Hedgehog as H
import qualified Hedgehog.Gen as HG
import qualified Hedgehog.Range as HR
import Test.Hspec (Spec, describe, it, runIO)
import Test.Hspec.Hedgehog (hedgehog)

import Passetto.JsonEncryption

import Test.Passetto.Util

spec :: Spec
spec = do
  keysCtx <- runIO newKeysContext

  describe "Single value" $ do
    it "Roundtrip" . hedgehog $ do
      payload <- H.forAll $ HG.text (HR.exponential 1 4096) HG.unicodeAll
      flip runReaderT keysCtx $ do
        res <- encryptPayload payload >>= runExceptT . decryptPayload
        res === Right payload

  describe "JSON object" $ do
    it "Roundtrip" . hedgehog $ do
      payload <- H.forAll genJson
      flip runReaderT keysCtx $ do
        res <- encryptJsonPayload payload >>= runExceptT . decryptJsonPayload
        res === Right payload
