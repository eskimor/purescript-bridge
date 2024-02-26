-- File auto generated by purescript-bridge! --
module Argonaut.Types where

import Prelude

import Control.Lazy (defer)
import Data.Argonaut.Aeson.Decode.Generic (genericDecodeAeson)
import Data.Argonaut.Aeson.Encode.Generic (genericEncodeAeson)
import Data.Argonaut.Aeson.Options (defaultOptions) as Argonaut
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson, class DecodeJsonField, decodeJson)
import Data.Argonaut.Decode.Class as Argonaut
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Class as Argonaut
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Either (Either)
import Data.Enum (class Enum)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Generic.Rep (class Generic)
import Data.Lens (Iso', Lens', Prism', iso, lens, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Foreign.Object (Object)
import Type.Proxy (Proxy(Proxy))

newtype Baz = Baz { _bazMessage :: String }

instance EncodeJson Baz where
  encodeJson = defer \_ -> genericEncodeAeson Argonaut.defaultOptions

instance DecodeJson Baz where
  decodeJson = defer \_ -> genericDecodeAeson Argonaut.defaultOptions



instance Show Baz where
  show a = genericShow a

derive instance Generic Baz _

derive instance Newtype Baz _

--------------------------------------------------------------------------------

_Baz :: Iso' Baz {_bazMessage :: String}
_Baz = _Newtype

bazMessage :: Lens' Baz String
bazMessage = _Newtype <<< prop (Proxy :: _"_bazMessage")

--------------------------------------------------------------------------------

data ID a = ID

instance (EncodeJson a) => EncodeJson (ID a) where
  encodeJson = defer \_ -> genericEncodeAeson Argonaut.defaultOptions

instance (DecodeJson a, DecodeJsonField a) => DecodeJson (ID a) where
  decodeJson = defer \_ -> genericDecodeAeson Argonaut.defaultOptions



instance Show (ID a) where
  show a = genericShow a

derive instance Generic (ID a) _

-- instance Enum (ID a) where
--   succ = genericSucc
--   pred = genericPred

-- instance Bounded (ID a) where
--   bottom = genericBottom
--   top = genericTop

--------------------------------------------------------------------------------

_ID :: forall a. Iso' (ID a) Unit
_ID = iso (const unit) (const ID)

--------------------------------------------------------------------------------

newtype ID2 a = ID2 { getID :: Int }

instance (EncodeJson a) => EncodeJson (ID2 a) where
  encodeJson = defer \_ -> genericEncodeAeson Argonaut.defaultOptions

instance (DecodeJson a, DecodeJsonField a) => DecodeJson (ID2 a) where
  decodeJson = defer \_ -> genericDecodeAeson Argonaut.defaultOptions



instance Show (ID2 a) where
  show a = genericShow a

derive instance Generic (ID2 a) _

derive instance Newtype (ID2 a) _

--------------------------------------------------------------------------------

_ID2 :: forall a. Iso' (ID2 a) {getID :: Int}
_ID2 = _Newtype

--------------------------------------------------------------------------------

newtype Foo = Foo
  { _fooMessage :: String
  , _fooE :: Either String Int
  , _fooNumber :: Int
  , _fooList :: Array Int
  , _fooMap :: Object Int
  , _fooBaz :: Baz
  , _fooTestSum :: TestSum
  , _fooTestData :: TestData
  }

instance EncodeJson Foo where
  encodeJson = defer \_ -> genericEncodeAeson Argonaut.defaultOptions

instance DecodeJson Foo where
  decodeJson = defer \_ -> genericDecodeAeson Argonaut.defaultOptions



instance Show Foo where
  show a = genericShow a

derive instance Generic Foo _

derive instance Newtype Foo _

--------------------------------------------------------------------------------

_Foo :: Iso' Foo {_fooMessage :: String, _fooE :: Either String Int, _fooNumber :: Int, _fooList :: Array Int, _fooMap :: Object Int, _fooBaz :: Baz, _fooTestSum :: TestSum, _fooTestData :: TestData}
_Foo = _Newtype

fooMessage :: Lens' Foo String
fooMessage = _Newtype <<< prop (Proxy :: _"_fooMessage")

fooE :: Lens' Foo (Either String Int)
fooE = _Newtype <<< prop (Proxy :: _"_fooE")

fooNumber :: Lens' Foo Int
fooNumber = _Newtype <<< prop (Proxy :: _"_fooNumber")

fooList :: Lens' Foo (Array Int)
fooList = _Newtype <<< prop (Proxy :: _"_fooList")

fooMap :: Lens' Foo (Object Int)
fooMap = _Newtype <<< prop (Proxy :: _"_fooMap")

fooBaz :: Lens' Foo Baz
fooBaz = _Newtype <<< prop (Proxy :: _"_fooBaz")

fooTestSum :: Lens' Foo TestSum
fooTestSum = _Newtype <<< prop (Proxy :: _"_fooTestSum")

fooTestData :: Lens' Foo TestData
fooTestData = _Newtype <<< prop (Proxy :: _"_fooTestData")

--------------------------------------------------------------------------------

newtype Bar a = Bar a

instance (EncodeJson a) => EncodeJson (Bar a) where
  encodeJson = defer \_ -> genericEncodeAeson Argonaut.defaultOptions

instance (DecodeJson a, DecodeJsonField a) => DecodeJson (Bar a) where
  decodeJson = defer \_ -> genericDecodeAeson Argonaut.defaultOptions



instance (Show a) => Show (Bar a) where
  show a = genericShow a

derive instance Generic (Bar a) _

derive instance Newtype (Bar a) _

--------------------------------------------------------------------------------

_Bar :: forall a. Iso' (Bar a) a
_Bar = _Newtype

--------------------------------------------------------------------------------

data TestSum
  = Nullary
  | Bool Boolean
  | Int Int
  | Number Number

instance EncodeJson TestSum where
  encodeJson = defer \_ -> genericEncodeAeson Argonaut.defaultOptions

instance DecodeJson TestSum where
  decodeJson = defer \_ -> genericDecodeAeson Argonaut.defaultOptions



instance Show TestSum where
  show a = genericShow a

derive instance Generic TestSum _

--------------------------------------------------------------------------------

_Nullary :: Prism' TestSum Unit
_Nullary = prism' (const Nullary) case _ of
  Nullary -> Just unit
  _ -> Nothing

_Bool :: Prism' TestSum Boolean
_Bool = prism' Bool case _ of
  (Bool a) -> Just a
  _ -> Nothing

_Int :: Prism' TestSum Int
_Int = prism' Int case _ of
  (Int a) -> Just a
  _ -> Nothing

_Number :: Prism' TestSum Number
_Number = prism' Number case _ of
  (Number a) -> Just a
  _ -> Nothing

--------------------------------------------------------------------------------

data TestData
  = TMaybe (Maybe TestSum)
  | TEither String

instance EncodeJson TestData where
  encodeJson = defer \_ -> genericEncodeAeson Argonaut.defaultOptions

instance DecodeJson TestData where
  decodeJson = defer \_ -> genericDecodeAeson Argonaut.defaultOptions



instance Show TestData where
  show a = genericShow a

derive instance Generic TestData _

--------------------------------------------------------------------------------

_TMaybe :: Prism' TestData (Maybe TestSum)
_TMaybe = prism' TMaybe case _ of
  (TMaybe a) -> Just a
  _ -> Nothing

_TEither :: Prism' TestData String
_TEither = prism' TEither case _ of
  (TEither a) -> Just a
  _ -> Nothing