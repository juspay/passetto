module Passetto.Util
  ( aesonOptions
  , encodeBase64
  , decodeBase64
  , encodeJsonT
  , decodeJsonT
  ) where

import Universum

import qualified Data.Aeson as Aeson
import Data.Aeson.Casing (snakeCase)
import Data.Aeson.TH as Aeson
import qualified Data.ByteString.Base64 as Base64

aesonOptions :: Aeson.Options
aesonOptions = defaultOptions { Aeson.fieldLabelModifier = snakeCase }

-- | It is safe to assume that returned value is text.
encodeBase64 :: ByteString -> Text
encodeBase64 = decodeUtf8 . Base64.encode

decodeBase64 :: Text -> Maybe ByteString
decodeBase64 = rightToMaybe . Base64.decode . encodeUtf8

-- | It is safe to assume that returned JSON is text.
encodeJsonT :: Aeson.ToJSON a => a -> Text
encodeJsonT = decodeUtf8 . Aeson.encode

decodeJsonT :: Aeson.FromJSON a => Text -> Either Text a
decodeJsonT = first toText . Aeson.eitherDecode . encodeUtf8
