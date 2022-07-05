{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Provides typeclass for encrypting arbitrary structures.
module Passetto.Client.EncryptedItem
  ( BulkCall
  , BulkM
  , bulkSend
  , bulkParsing
  , runBulkM

  , Encrypted(..)
  , coerceEncrypted
  , EncryptedBase64 (..)

  , EncryptedItem (..)
  , EncryptedRaw
  , BulkEncryptCall
  , EncryptM
  , BulkDecryptCall
  , DecryptM
  , genericEncryptItem
  , genericDecryptItem
  , genericEncryptItemFully
  , genericDecryptItemFully
  , EncryptedOrPlainItem
  ) where

import Universum

import Control.Monad.Except (MonadError(..))
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import GHC.Generics ((:*:)(..), (:+:))
import qualified GHC.Generics as G
import GHC.TypeLits (ErrorMessage(..), TypeError)
import Servant.API (FromHttpApiData, ToHttpApiData)

import Passetto.Client.Bulk
import Passetto.Util

-- | Defines a relation between a plain and encrypted data.
--
-- Atomic values are turned into JSON or some other encoding upon encryption.
--
-- More complex structures like 'Maybe' or tuples can be encrypted on
-- element-wise basis or as a whole, depending on the type of the encryption
-- result
-- (compare encrypting to @[Encrypted Int]@ vs encrypting to @Encrypted [Int]@).
--
-- When defining an instance for your type, use the following rule:
--
-- 1. If your type is atomic, use existing 'Encrypted' for it which requires JSON
--    instances; if that does not suit, define a newtype wrapper and
--    @EncryptedItem@ instance for it which would use its custom encoding.
--    Do not define an instance for your type directly.
--
-- 2. Complex types which group other types can have @EncrytedItem@ instance
--    and use 'genericEncryptItem' and 'genericDecryptItem' as implementation.
--
-- 3. Types which do not require encryption (e.g. '()') can also have
--    @EncryptedItem@ instance and supply implementation manually.
class EncryptedItem e where
  type Unencrypted e :: Type
  encryptItem :: Unencrypted e -> EncryptM e
  decryptItem :: e -> DecryptM (Unencrypted e)

type family ForbidEq ty :: Constraint where
  ForbidEq ty = TypeError
    ('Text "Using Eq instance for `" ':<>: 'ShowType ty ':<>:
    'Text "` is not allowed" ':$$:
    'Text "Encryption is not a deterministic operation"
    )

-- | Stands for an encrypted value of type @a@, serialized with JSON instances
-- inside.
--
-- The content of this type is a base64-encoded text.
--
-- If instead of JSON instances you want to use some other encoding,
-- add a newtype wrapper specific for it (see e.g. 'EncryptedBase64').
newtype Encrypted a = Encrypted { unEncrypted :: Text }
  deriving stock (Show)
  deriving newtype (ToJSON, FromJSON, ToHttpApiData, FromHttpApiData)

deriving newtype instance ForbidEq (Encrypted a) => Eq (Encrypted a)

coerceEncrypted :: Encrypted a -> Encrypted b
coerceEncrypted = Encrypted . unEncrypted

instance (ToJSON a, FromJSON a) => EncryptedItem (Encrypted a) where
  type Unencrypted (Encrypted a) = a
  encryptItem a = Encrypted . unEncryptedRaw <$> bulkSend (Aeson.toJSON a)
  decryptItem e =
    bulkParsing (bulkSend . EncryptedRaw $ unEncrypted e) $ \v ->
      case Aeson.fromJSON v of
        Aeson.Success x -> pure x
        Aeson.Error err -> throwError ("JSON decode error: " <> toText err)

-- | This is similar to 'Encrypted', but stands for base64 of binary data
-- being encrypted.
newtype EncryptedBase64 = EncryptedBase64 { unBase64Encrypted :: Text }
  deriving stock (Show)
  deriving newtype (ToJSON, FromJSON, ToHttpApiData, FromHttpApiData)

deriving newtype instance ForbidEq EncryptedBase64 => Eq EncryptedBase64

instance EncryptedItem EncryptedBase64 where
  type Unencrypted EncryptedBase64 = ByteString
  encryptItem a =
    EncryptedBase64 . unEncrypted <$>
    encryptItem @(Encrypted Text) (encodeBase64 a)
  decryptItem e =
    bulkParsing
      (decryptItem (Encrypted $ unBase64Encrypted e))
      (maybeToRight "Bad base64" . decodeBase64)

instance EncryptedItem e => EncryptedItem (Maybe e) where
  type Unencrypted (Maybe e) = Maybe (Unencrypted e)
  encryptItem = traverse encryptItem
  decryptItem = traverse decryptItem

instance (EncryptedItem e1, EncryptedItem e2) =>
         EncryptedItem (Either e1 e2) where
  type Unencrypted (Either e1 e2) = Either (Unencrypted e1) (Unencrypted e2)
  encryptItem = genericEncryptItemFully
  decryptItem = genericDecryptItemFully

instance EncryptedItem Void where
  type Unencrypted Void = Void
  encryptItem = genericEncryptItem
  decryptItem = genericDecryptItem

instance EncryptedItem e =>
         EncryptedItem [e] where
  type Unencrypted [e] = [Unencrypted e]
  encryptItem = traverse encryptItem
  decryptItem = traverse decryptItem

instance EncryptedItem () where
  type Unencrypted () = ()
  encryptItem = pure
  decryptItem = pure

instance (EncryptedItem ea, EncryptedItem eb) =>
         EncryptedItem (ea, eb) where
  type Unencrypted (ea, eb) = (Unencrypted ea, Unencrypted eb)
  encryptItem = genericEncryptItemFully
  decryptItem = genericDecryptItemFully

instance ( EncryptedItem ea, EncryptedItem eb
         , EncryptedItem ec
         ) =>
         EncryptedItem (ea, eb, ec) where
  type Unencrypted (ea, eb, ec) =
    (Unencrypted ea, Unencrypted eb, Unencrypted ec)
  encryptItem = genericEncryptItemFully
  decryptItem = genericDecryptItemFully

instance ( EncryptedItem ea, EncryptedItem eb
         , EncryptedItem ec, EncryptedItem ed
         ) =>
         EncryptedItem (ea, eb, ec, ed) where
  type Unencrypted (ea, eb, ec, ed) =
    (Unencrypted ea, Unencrypted eb, Unencrypted ec, Unencrypted ed)
  encryptItem = genericEncryptItemFully
  decryptItem = genericDecryptItemFully

instance ( EncryptedItem ea, EncryptedItem eb
         , EncryptedItem ec, EncryptedItem ed
         , EncryptedItem ee
         ) =>
         EncryptedItem (ea, eb, ec, ed, ee) where
  type Unencrypted (ea, eb, ec, ed, ee) =
    ( Unencrypted ea, Unencrypted eb, Unencrypted ec
    , Unencrypted ed, Unencrypted ee
    )
  encryptItem = genericEncryptItemFully
  decryptItem = genericDecryptItemFully

instance ( EncryptedItem ea, EncryptedItem eb
         , EncryptedItem ec, EncryptedItem ed
         , EncryptedItem ee, EncryptedItem ef
         ) =>
         EncryptedItem (ea, eb, ec, ed, ee, ef) where
  type Unencrypted (ea, eb, ec, ed, ee, ef) =
    ( Unencrypted ea, Unencrypted eb, Unencrypted ec
    , Unencrypted ed, Unencrypted ee, Unencrypted ef
    )
  encryptItem = genericEncryptItemFully
  decryptItem = genericDecryptItemFully

-- ** Generic instance

-- | Derive 'encryptItem' for a datatype with 'Generic' instance.
--
-- This is suitable for case when your datatype is polymorphic and
-- some fields are sometimes encrypted, sometimes not - this function
-- automatically encrypts the necessary fields, skipping those which
-- are of the same type in both versions of the datatype.
--
-- It best works if all your fields are not completely polymorphic,
-- otherwise, you may need to add some 'EncryptedOrPlainItem' constraints
-- to superclass of your instance.
--
-- This cannot transfer between different datatypes, even if they have
-- similar sets of fields.
genericEncryptItem
  :: (Generic e, Generic a, GEncryptedGeneral 'SkipMatching (G.Rep e) (G.Rep a))
  => a -> EncryptM e
genericEncryptItem = fmap G.to . gEncryptItem @'SkipMatching . G.from

-- | Derive 'decryptItem' similarly to 'genericEncryptItem'.
genericDecryptItem
  :: (Generic e, Generic a, GEncryptedGeneral 'SkipMatching (G.Rep e) (G.Rep a))
  => e -> DecryptM a
genericDecryptItem = fmap G.to . gDecryptItem @'SkipMatching . G.from

-- | This is similar to 'genericEncryptItem', but always encrypts/decrypts
-- all the fields. This is suitable for control structures like tuples or
-- 'Maybe'. It does not have problems with polymorphic fields which
-- 'genericEncryptItem' has.
genericEncryptItemFully
  :: (Generic e, Generic a, GEncryptedGeneral 'ConvertAll (G.Rep e) (G.Rep a))
  => a -> EncryptM e
genericEncryptItemFully = fmap G.to . gEncryptItem @'ConvertAll . G.from

-- | Derive 'decryptItem' similarly to 'genericEncryptItemFully'.
genericDecryptItemFully
  :: (Generic e, Generic a, GEncryptedGeneral 'ConvertAll (G.Rep e) (G.Rep a))
  => e -> DecryptM a
genericDecryptItemFully = fmap G.to . gDecryptItem @'ConvertAll . G.from

-- | How do we handle fields in generic derivation?
data GenericMode
  = SkipMatching
    -- ^ Fields matching in encrypted and plaintext datatype are skipped.
  | ConvertAll
    -- ^ All fields participate in encryption.

class GEncryptedGeneral (mode :: GenericMode) ex x where
  gEncryptItem :: x p -> EncryptM (ex p)
  gDecryptItem :: ex p -> DecryptM (x p)

instance GEncryptedGeneral m ex x =>
         GEncryptedGeneral m (G.M1 t i ex) (G.M1 t i x) where
  gEncryptItem (G.M1 x) = G.M1 <$> gEncryptItem @m x
  gDecryptItem (G.M1 x) = G.M1 <$> gDecryptItem @m x

instance (GEncryptedGeneral m ex x, GEncryptedGeneral m ey y) =>
         GEncryptedGeneral m (ex :*: ey) (x :*: y) where
  gEncryptItem (x :*: y) = (:*:) <$> gEncryptItem @m x <*> gEncryptItem @m y
  gDecryptItem (x :*: y) = (:*:) <$> gDecryptItem @m x <*> gDecryptItem @m y

instance GEncryptedGeneral m G.U1 G.U1 where
  gEncryptItem G.U1 = pure G.U1
  gDecryptItem G.U1 = pure G.U1

instance (GEncryptedGeneral m ex x, GEncryptedGeneral m ey y) =>
         GEncryptedGeneral m (ex :+: ey) (x :+: y) where
  gEncryptItem = \case
    G.L1 x -> G.L1 <$> gEncryptItem @m x
    G.R1 x -> G.R1 <$> gEncryptItem @m x
  gDecryptItem = \case
    G.L1 x -> G.L1 <$> gDecryptItem @m x
    G.R1 x -> G.R1 <$> gDecryptItem @m x

instance GEncryptedGeneral m G.V1 G.V1 where
  gEncryptItem v = case v of {}
  gDecryptItem v = case v of {}

instance (EncryptedItem ex, Unencrypted ex ~ x) =>
         GEncryptedGeneral 'ConvertAll (G.Rec0 ex) (G.Rec0 x) where
  gEncryptItem (G.K1 a) = G.K1 <$> encryptItem a
  gDecryptItem (G.K1 e) = G.K1 <$> decryptItem e

instance (EncryptedOrPlainItem ex x) =>
         GEncryptedGeneral 'SkipMatching (G.Rec0 ex) (G.Rec0 x) where
  gEncryptItem (G.K1 a) = G.K1 <$> encryptOrRetainItem @ex @x a
  gDecryptItem (G.K1 e) = G.K1 <$> decryptOrRetainItem @ex @x e

-- | Does nothing if the argument and result are the same type,
-- otherwise assumes one to be encryption of another.
class EncryptedOrPlainItem e a where
  encryptOrRetainItem :: a -> EncryptM e
  decryptOrRetainItem :: e -> DecryptM a

instance {-# OVERLAPPING #-} EncryptedOrPlainItem x x where
  encryptOrRetainItem = pure
  decryptOrRetainItem = pure

instance (EncryptedItem e, Unencrypted e ~ a) => EncryptedOrPlainItem e a where
  encryptOrRetainItem = encryptItem
  decryptOrRetainItem = decryptItem
