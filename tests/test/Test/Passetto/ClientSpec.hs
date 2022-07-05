{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- | Tests on high-level passetto client functionality.
module Test.Passetto.ClientSpec (spec) where

import Universum hiding (keys)

import qualified Data.Aeson as Aeson
import Data.Typeable (typeRep)
import Hedgehog (Gen, PropertyT, evalEither, evalM, forAll, (===))
import qualified Hedgehog.Gen as HG
import qualified Hedgehog.Range as HR
import Network.Wai.Handler.Warp (Port, testWithApplication)
import Test.Hspec (Spec, SpecWith, around, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

import Passetto.Client
import Passetto.Client.EncryptedItem
import Passetto.Crypto (Password, passwordFromText)
import Passetto.Db
import Passetto.KeysContext
import Passetto.Version
import Passetto.Web.Server

import Test.Passetto.Util

masterPassword :: Password
masterPassword = passwordFromText "123"

withRunServer :: (Port -> IO a) -> IO a
withRunServer action = do
  acidState <- openDb
  appendDb masterPassword 3 acidState
    >>= either throwM pure
  keys <- readAndDecryptDb masterPassword acidState
    >>= either throwM pure
  let keysCtx = KeysContext
        { storedKeys = keys
        , modelVersion = encryptedContentVersion
        }
  testWithApplication (pure $ server keysCtx) action

withClientCtx :: (PassettoContext -> IO a) -> IO a
withClientCtx action =
  withRunServer $ \port ->
    mkDefPassettoContext "localhost" (fromIntegral port) >>= action

runCli :: PassettoContext -> ReaderT PassettoContext m a -> m a
runCli ctx action = runReaderT action ctx

data EncType a = EncType
  { etInt :: Int
  , etItem :: a
  } deriving stock (Eq, Show, Generic)

instance (EncryptedOrPlainItem e (Unencrypted e)) =>
         EncryptedItem (EncType e) where
  type Unencrypted (EncType e) = EncType (Unencrypted e)
  encryptItem = genericEncryptItem
  decryptItem = genericDecryptItem

roundtripProp
  :: forall e a.
      (Show a, Eq a, EncryptedItem e, Unencrypted e ~ a)
  => PassettoContext -> Gen a -> PropertyT IO ()
roundtripProp cliCtx gen = hedgehog $ do
  val <- forAll gen
  val' <- runCli cliCtx $ do
    enc <- cliEncrypt @e val >>= evalEither
    cliDecrypt enc >>= evalEither
  val' === val

roundtrip
  :: forall e a.
      (Typeable a, Show a, Eq a, EncryptedItem e, Unencrypted e ~ a)
  => Gen a -> SpecWith PassettoContext
roundtrip gen =
  it (show (typeRep $ Proxy @a)) $ \cliCtx ->
    hedgehog $ roundtripProp @e cliCtx gen

spec :: Spec
spec = do
  around withClientCtx $ do
    describe "Plain encrypt" $ do
      describe "Roundtrip" $ do
        roundtrip @(Encrypted Int) $ HG.int (HR.linear 1 10)
        roundtrip @(Encrypted Text) $ HG.text (HR.linear 1 10) HG.unicodeAll
        roundtrip @(Encrypted Bool) $ HG.bool
        roundtrip @(Encrypted [Bool]) $ HG.list (HR.linear 1 10) HG.bool
        roundtrip @(Encrypted Aeson.Value) $ genJson

    describe "Binary encrypt" $ do
      describe "Roundtrip" $ do
        roundtrip @EncryptedBase64 (HG.bytes $ HR.linear 0 100)

    describe "Bulk encrypt" $ do
      describe "Roundtrip" $ do
        roundtrip @[Encrypted Aeson.Value] $
          HG.list (HR.linear 1 3) genJson

        roundtrip @(Encrypted Int, Encrypted Text) $
          (,) <$> HG.int (HR.linear 1 10)
              <*> HG.text (HR.linear 1 10) HG.unicodeAll

    describe "Generic encryption" $ do
      it "Roundtrip" $ \cliCtx -> hedgehog $
        roundtripProp @(EncType $ Encrypted Int) cliCtx $
          EncType <$> HG.int (HR.linear 1 10) <*> HG.int (HR.linear 1 10)

  it "Empty submission does not require connection to the server" $ do
    fakeCliCtx <- mkDefPassettoContext "localhost" 8999
    hedgehog . runCli fakeCliCtx $ do
      enc <- evalM $ cliEncrypt @(Maybe (Encrypted Int), ()) (Nothing, ())
                     >>= evalEither
      void . evalM $ cliDecrypt enc >>= evalEither
