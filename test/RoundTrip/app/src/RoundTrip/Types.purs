-- File auto generated by purescript-bridge! --
module RoundTrip.Types where

import Prelude

import Control.Lazy (defer)
import Data.Argonaut.Aeson.Decode.Generic (genericDecodeAeson)
import Data.Argonaut.Aeson.Encode.Generic (genericEncodeAeson)
import Data.Argonaut.Aeson.Options (defaultOptions) as Argonaut
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson, class DecodeJsonField, decodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Either (Either)
import Data.Enum (class Enum)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Generic.Rep (class Generic)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple)
import Foreign.Object (Object)
import Type.Proxy (Proxy(Proxy))

data TestData
  = Maybe (Maybe TestSum)
  | Either (Either (Maybe Int) (Maybe Boolean))

derive instance Eq TestData

derive instance Ord TestData

instance Show TestData where
  show a = genericShow a

instance EncodeJson TestData where
  encodeJson = defer \_ -> genericEncodeAeson Argonaut.defaultOptions

instance DecodeJson TestData where
  decodeJson = defer \_ -> genericDecodeAeson Argonaut.defaultOptions

derive instance Generic TestData _

--------------------------------------------------------------------------------

data TestSum
  = Nullary
  | Bool Boolean
  | Int Int
  | Number Number
  | String String
  | Array (Array Int)
  | InlineRecord
    { why :: String
    , wouldYouDoThis :: Int
    }
  | MultiInlineRecords TestMultiInlineRecords
  | Record (TestRecord Int)
  | NestedRecord (TestRecord (TestRecord Int))
  | NT TestNewtype
  | NTRecord TestNewtypeRecord
  | TwoFields TestTwoFields
  | Set (Set Int)
  | Map (Object Int)
  | Unit Unit
  | MyUnit MyUnit
  | Pair (Tuple Int Number)
  | Triple (Tuple Int (Tuple Unit Boolean))
  | Quad (Tuple Int (Tuple Number (Tuple Boolean Number)))
  | QuadSimple Int Number Boolean Number
  | Recursive TestRecursiveA
  | Enum TestEnum

derive instance Eq TestSum

derive instance Ord TestSum

instance Show TestSum where
  show a = genericShow a

instance EncodeJson TestSum where
  encodeJson = defer \_ -> genericEncodeAeson Argonaut.defaultOptions

instance DecodeJson TestSum where
  decodeJson = defer \_ -> genericDecodeAeson Argonaut.defaultOptions

derive instance Generic TestSum _

--------------------------------------------------------------------------------

data TestRecursiveA
  = Nil
  | Recurse TestRecursiveB

derive instance Eq TestRecursiveA

derive instance Ord TestRecursiveA

instance Show TestRecursiveA where
  show a = genericShow a

instance EncodeJson TestRecursiveA where
  encodeJson = defer \_ -> genericEncodeAeson Argonaut.defaultOptions

instance DecodeJson TestRecursiveA where
  decodeJson = defer \_ -> genericDecodeAeson Argonaut.defaultOptions

derive instance Generic TestRecursiveA _

--------------------------------------------------------------------------------

newtype TestRecursiveB = RecurseB TestRecursiveB

derive instance Eq TestRecursiveB

derive instance Ord TestRecursiveB

instance Show TestRecursiveB where
  show a = genericShow a

instance EncodeJson TestRecursiveB where
  encodeJson = defer \_ -> genericEncodeAeson Argonaut.defaultOptions

instance DecodeJson TestRecursiveB where
  decodeJson = defer \_ -> genericDecodeAeson Argonaut.defaultOptions

derive instance Generic TestRecursiveB _

derive instance Newtype TestRecursiveB _

--------------------------------------------------------------------------------

newtype TestRecord a = TestRecord
  { _field1 :: Maybe Int
  , _field2 :: a
  }

derive instance Functor TestRecord

derive instance (Eq a) => Eq (TestRecord a)

derive instance (Ord a) => Ord (TestRecord a)

instance (Show a) => Show (TestRecord a) where
  show a = genericShow a

instance (EncodeJson a) => EncodeJson (TestRecord a) where
  encodeJson = defer \_ -> genericEncodeAeson Argonaut.defaultOptions

instance (DecodeJson a, DecodeJsonField a) => DecodeJson (TestRecord a) where
  decodeJson = defer \_ -> genericDecodeAeson Argonaut.defaultOptions

derive instance Generic (TestRecord a) _

derive instance Newtype (TestRecord a) _

--------------------------------------------------------------------------------

newtype TestNewtype = TestNewtype (TestRecord Boolean)

derive instance Eq TestNewtype

derive instance Ord TestNewtype

instance Show TestNewtype where
  show a = genericShow a

instance EncodeJson TestNewtype where
  encodeJson = defer \_ -> genericEncodeAeson Argonaut.defaultOptions

instance DecodeJson TestNewtype where
  decodeJson = defer \_ -> genericDecodeAeson Argonaut.defaultOptions

derive instance Generic TestNewtype _

derive instance Newtype TestNewtype _

--------------------------------------------------------------------------------

newtype TestNewtypeRecord = TestNewtypeRecord { unTestNewtypeRecord :: TestNewtype }

derive instance Eq TestNewtypeRecord

derive instance Ord TestNewtypeRecord

instance Show TestNewtypeRecord where
  show a = genericShow a

instance EncodeJson TestNewtypeRecord where
  encodeJson = defer \_ -> genericEncodeAeson Argonaut.defaultOptions

instance DecodeJson TestNewtypeRecord where
  decodeJson = defer \_ -> genericDecodeAeson Argonaut.defaultOptions

derive instance Generic TestNewtypeRecord _

derive instance Newtype TestNewtypeRecord _

--------------------------------------------------------------------------------

data TestMultiInlineRecords
  = Foo
    { _foo1 :: Maybe Int
    , _foo2 :: Unit
    }
  | Bar
    { _bar1 :: String
    , _bar2 :: Boolean
    }

derive instance Eq TestMultiInlineRecords

derive instance Ord TestMultiInlineRecords

instance Show TestMultiInlineRecords where
  show a = genericShow a

instance EncodeJson TestMultiInlineRecords where
  encodeJson = defer \_ -> genericEncodeAeson Argonaut.defaultOptions

instance DecodeJson TestMultiInlineRecords where
  decodeJson = defer \_ -> genericDecodeAeson Argonaut.defaultOptions

derive instance Generic TestMultiInlineRecords _

--------------------------------------------------------------------------------

data TestTwoFields = TestTwoFields Boolean Int

derive instance Eq TestTwoFields

derive instance Ord TestTwoFields

instance Show TestTwoFields where
  show a = genericShow a

instance EncodeJson TestTwoFields where
  encodeJson = defer \_ -> genericEncodeAeson Argonaut.defaultOptions

instance DecodeJson TestTwoFields where
  decodeJson = defer \_ -> genericDecodeAeson Argonaut.defaultOptions

derive instance Generic TestTwoFields _

--------------------------------------------------------------------------------

data TestEnum
  = Mon
  | Tue
  | Wed
  | Thu
  | Fri
  | Sat
  | Sun

derive instance Eq TestEnum

derive instance Ord TestEnum

instance Show TestEnum where
  show a = genericShow a

instance EncodeJson TestEnum where
  encodeJson = defer \_ -> genericEncodeAeson Argonaut.defaultOptions

instance DecodeJson TestEnum where
  decodeJson = defer \_ -> genericDecodeAeson Argonaut.defaultOptions

derive instance Generic TestEnum _

instance Enum TestEnum where
  succ = genericSucc
  pred = genericPred

instance Bounded TestEnum where
  bottom = genericBottom
  top = genericTop

--------------------------------------------------------------------------------

data MyUnit = U

derive instance Eq MyUnit

derive instance Ord MyUnit

instance Show MyUnit where
  show a = genericShow a

instance EncodeJson MyUnit where
  encodeJson = defer \_ -> genericEncodeAeson Argonaut.defaultOptions

instance DecodeJson MyUnit where
  decodeJson = defer \_ -> genericDecodeAeson Argonaut.defaultOptions

derive instance Generic MyUnit _

instance Enum MyUnit where
  succ = genericSucc
  pred = genericPred

instance Bounded MyUnit where
  bottom = genericBottom
  top = genericTop
