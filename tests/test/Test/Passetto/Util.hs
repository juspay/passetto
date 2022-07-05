module Test.Passetto.Util
  ( newKeysContext
  , genJson
  ) where

import Universum

import qualified Crypto.Saltine.Core.Box as Box
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.SemVer as V
import GHC.Exts (fromList)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as HG
import qualified Hedgehog.Range as HR

import Passetto.Crypto
import Passetto.KeysContext

-- | Make a sample of keys context.
newKeysContext :: MonadIO m => m KeysContext
newKeysContext = liftIO $ do
  sodiumInit
  let modelVersion = V.initial
  storedKeys <- fromList <$> replicateM 3 Box.newKeypair
  return KeysContext{..}

-- We could use hedgehog-gen-json package, but it requires too many dependencies
-- not from LTS
genJson :: H.Gen A.Value
genJson = HG.recursive HG.choice
  [ A.String <$> HG.text (HR.linear 1 5) HG.unicodeAll
  , A.Number <$> HG.realFrac_ (HR.constant (-100) 100)
  , A.Bool <$> HG.bool
  , pure A.Null
  ]
  [ A.Array . fromList <$> HG.list (HR.linear 1 3) genJson
  , A.Object . HM.fromList <$> HG.list (HR.linear 1 3) genKeyValue
  ]
  where
    genKeyValue :: H.Gen (Text, A.Value)
    genKeyValue = (,) <$> HG.text (HR.constant 1 5) HG.unicodeAll <*> genJson
