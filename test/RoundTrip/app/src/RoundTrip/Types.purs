-- File auto generated by purescript-bridge! --
module RoundTrip.Types where

import Prelude

import Control.Lazy (defer)
import Data.Argonaut (encodeJson, jsonNull)
import Data.Argonaut.Aeson.Decode.Generic (genericDecodeAeson)
import Data.Argonaut.Aeson.Encode.Generic (genericEncodeAeson)
import Data.Argonaut.Aeson.Options (defaultOptions)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Aeson ((</$\>), (</*\>), (</\>))
import Data.Argonaut.Decode.Class (class DecodeJson, class DecodeJsonField, decodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Aeson ((>$<), (>/\<))
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Either (Either)
import Data.Enum (class Enum)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Generic.Rep (class Generic)
import Data.Lens (Iso', Lens', Prism', iso, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Set (Set)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Foreign.Object (Object)
import Type.Proxy (Proxy(Proxy))
import Data.Argonaut.Decode.Aeson as D
import Data.Argonaut.Encode.Aeson as E
import Data.Map as Map

data TestData
  = Maybe (Maybe TestSum)
  | Either (Either (Maybe Int) (Maybe Boolean))

derive instance Eq TestData

instance Show TestData where
  show a = genericShow a

derive instance Ord TestData

instance EncodeJson TestData where
  encodeJson = defer \_ -> case _ of
    Maybe a -> E.encodeTagged "Maybe" a (E.maybe E.value)
    Either a -> E.encodeTagged "Either" a (E.either (E.maybe E.value) (E.maybe E.value))

instance DecodeJson TestData where
  decodeJson = defer \_ -> D.decode
    $ D.sumType "TestData" $ Map.fromFoldable
      [ "Maybe" /\ D.content (Maybe <$> (D.maybe D.value))
      , "Either" /\ D.content (Either <$> (D.either (D.maybe D.value) (D.maybe D.value)))
      ]

derive instance Generic TestData _

--------------------------------------------------------------------------------

_Maybe :: Prism' TestData (Maybe TestSum)
_Maybe = prism' Maybe case _ of
  (Maybe a) -> Just a
  _ -> Nothing

_Either :: Prism' TestData (Either (Maybe Int) (Maybe Boolean))
_Either = prism' Either case _ of
  (Either a) -> Just a
  _ -> Nothing

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

instance Show TestSum where
  show a = genericShow a

derive instance Ord TestSum

instance EncodeJson TestSum where
  encodeJson = defer \_ -> case _ of
    Nullary -> encodeJson { tag: "Nullary", contents: jsonNull }
    Bool a -> E.encodeTagged "Bool" a E.value
    Int a -> E.encodeTagged "Int" a E.value
    Number a -> E.encodeTagged "Number" a E.value
    String a -> E.encodeTagged "String" a E.value
    Array a -> E.encodeTagged "Array" a E.value
    InlineRecord {why, wouldYouDoThis} -> encodeJson
      { tag: "InlineRecord"
      , why: flip E.encode why E.value
      , wouldYouDoThis: flip E.encode wouldYouDoThis E.value
      }
    MultiInlineRecords a -> E.encodeTagged "MultiInlineRecords" a E.value
    Record a -> E.encodeTagged "Record" a E.value
    NestedRecord a -> E.encodeTagged "NestedRecord" a E.value
    NT a -> E.encodeTagged "NT" a E.value
    NTRecord a -> E.encodeTagged "NTRecord" a E.value
    TwoFields a -> E.encodeTagged "TwoFields" a E.value
    Set a -> E.encodeTagged "Set" a E.value
    Map a -> E.encodeTagged "Map" a E.value
    Unit a -> E.encodeTagged "Unit" a E.unit
    MyUnit a -> E.encodeTagged "MyUnit" a E.value
    Pair a -> E.encodeTagged "Pair" a (E.tuple (E.value >/\< E.value))
    Triple a -> E.encodeTagged "Triple" a (E.tuple (E.value >/\< E.unit >/\< E.value))
    Quad a -> E.encodeTagged "Quad" a (E.tuple (E.value >/\< E.value >/\< E.value >/\< E.value))
    QuadSimple a b c d -> E.encodeTagged "QuadSimple" (a /\ b /\ c /\ d) (E.tuple (E.value >/\< E.value >/\< E.value >/\< E.value))
    Recursive a -> E.encodeTagged "Recursive" a E.value
    Enum a -> E.encodeTagged "Enum" a E.value

instance DecodeJson TestSum where
  decodeJson = defer \_ -> D.decode
    $ D.sumType "TestSum" $ Map.fromFoldable
      [ "Nullary" /\ pure Nullary
      , "Bool" /\ D.content (Bool <$> D.value)
      , "Int" /\ D.content (Int <$> D.value)
      , "Number" /\ D.content (Number <$> D.value)
      , "String" /\ D.content (String <$> D.value)
      , "Array" /\ D.content (Array <$> D.value)
      , "InlineRecord" /\ (InlineRecord <$> D.object "InlineRecord"
        { why: D.value :: _ String
        , wouldYouDoThis: D.value :: _ Int
        })
      , "MultiInlineRecords" /\ D.content (MultiInlineRecords <$> D.value)
      , "Record" /\ D.content (Record <$> D.value)
      , "NestedRecord" /\ D.content (NestedRecord <$> D.value)
      , "NT" /\ D.content (NT <$> D.value)
      , "NTRecord" /\ D.content (NTRecord <$> D.value)
      , "TwoFields" /\ D.content (TwoFields <$> D.value)
      , "Set" /\ D.content (Set <$> D.value)
      , "Map" /\ D.content (Map <$> D.value)
      , "Unit" /\ D.content (Unit <$> D.unit)
      , "MyUnit" /\ D.content (MyUnit <$> D.value)
      , "Pair" /\ D.content (Pair <$> (D.tuple (D.value </\> D.value)))
      , "Triple" /\ D.content (Triple <$> (D.tuple (D.value </\> D.unit </\> D.value)))
      , "Quad" /\ D.content (Quad <$> (D.tuple (D.value </\> D.value </\> D.value </\> D.value)))
      , "QuadSimple" /\ D.content (D.tuple $ QuadSimple </$\>D.value </*\> D.value </*\> D.value </*\> D.value)
      , "Recursive" /\ D.content (Recursive <$> D.value)
      , "Enum" /\ D.content (Enum <$> D.value)
      ]

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

_String :: Prism' TestSum String
_String = prism' String case _ of
  (String a) -> Just a
  _ -> Nothing

_Array :: Prism' TestSum (Array Int)
_Array = prism' Array case _ of
  (Array a) -> Just a
  _ -> Nothing

_InlineRecord :: Prism' TestSum {why :: String, wouldYouDoThis :: Int}
_InlineRecord = prism' InlineRecord case _ of
  (InlineRecord a) -> Just a
  _ -> Nothing

_MultiInlineRecords :: Prism' TestSum TestMultiInlineRecords
_MultiInlineRecords = prism' MultiInlineRecords case _ of
  (MultiInlineRecords a) -> Just a
  _ -> Nothing

_Record :: Prism' TestSum (TestRecord Int)
_Record = prism' Record case _ of
  (Record a) -> Just a
  _ -> Nothing

_NestedRecord :: Prism' TestSum (TestRecord (TestRecord Int))
_NestedRecord = prism' NestedRecord case _ of
  (NestedRecord a) -> Just a
  _ -> Nothing

_NT :: Prism' TestSum TestNewtype
_NT = prism' NT case _ of
  (NT a) -> Just a
  _ -> Nothing

_NTRecord :: Prism' TestSum TestNewtypeRecord
_NTRecord = prism' NTRecord case _ of
  (NTRecord a) -> Just a
  _ -> Nothing

_TwoFields :: Prism' TestSum TestTwoFields
_TwoFields = prism' TwoFields case _ of
  (TwoFields a) -> Just a
  _ -> Nothing

_Set :: Prism' TestSum (Set Int)
_Set = prism' Set case _ of
  (Set a) -> Just a
  _ -> Nothing

_Map :: Prism' TestSum (Object Int)
_Map = prism' Map case _ of
  (Map a) -> Just a
  _ -> Nothing

_Unit :: Prism' TestSum Unit
_Unit = prism' Unit case _ of
  (Unit a) -> Just a
  _ -> Nothing

_MyUnit :: Prism' TestSum MyUnit
_MyUnit = prism' MyUnit case _ of
  (MyUnit a) -> Just a
  _ -> Nothing

_Pair :: Prism' TestSum (Tuple Int Number)
_Pair = prism' Pair case _ of
  (Pair a) -> Just a
  _ -> Nothing

_Triple :: Prism' TestSum (Tuple Int (Tuple Unit Boolean))
_Triple = prism' Triple case _ of
  (Triple a) -> Just a
  _ -> Nothing

_Quad :: Prism' TestSum (Tuple Int (Tuple Number (Tuple Boolean Number)))
_Quad = prism' Quad case _ of
  (Quad a) -> Just a
  _ -> Nothing

_QuadSimple :: Prism' TestSum {a :: Int, b :: Number, c :: Boolean, d :: Number}
_QuadSimple = prism' (\{a, b, c, d} -> (QuadSimple a b c d)) case _ of
  (QuadSimple a b c d) -> Just {a, b, c, d}
  _ -> Nothing

_Recursive :: Prism' TestSum TestRecursiveA
_Recursive = prism' Recursive case _ of
  (Recursive a) -> Just a
  _ -> Nothing

_Enum :: Prism' TestSum TestEnum
_Enum = prism' Enum case _ of
  (Enum a) -> Just a
  _ -> Nothing

--------------------------------------------------------------------------------

data TestRecursiveA
  = Nil
  | Recurse TestRecursiveB

derive instance Eq TestRecursiveA

instance Show TestRecursiveA where
  show a = genericShow a

derive instance Ord TestRecursiveA

instance EncodeJson TestRecursiveA where
  encodeJson = defer \_ -> case _ of
    Nil -> encodeJson { tag: "Nil", contents: jsonNull }
    Recurse a -> E.encodeTagged "Recurse" a E.value

instance DecodeJson TestRecursiveA where
  decodeJson = defer \_ -> D.decode
    $ D.sumType "TestRecursiveA" $ Map.fromFoldable
      [ "Nil" /\ pure Nil
      , "Recurse" /\ D.content (Recurse <$> D.value)
      ]

derive instance Generic TestRecursiveA _

--------------------------------------------------------------------------------

_Nil :: Prism' TestRecursiveA Unit
_Nil = prism' (const Nil) case _ of
  Nil -> Just unit
  _ -> Nothing

_Recurse :: Prism' TestRecursiveA TestRecursiveB
_Recurse = prism' Recurse case _ of
  (Recurse a) -> Just a
  _ -> Nothing

--------------------------------------------------------------------------------

newtype TestRecursiveB = RecurseB TestRecursiveB

derive instance Eq TestRecursiveB

instance Show TestRecursiveB where
  show a = genericShow a

derive instance Ord TestRecursiveB

instance EncodeJson TestRecursiveB where
  encodeJson = defer \_ -> E.encode $ unwrap >$< E.value

instance DecodeJson TestRecursiveB where
  decodeJson = defer \_ -> D.decode $ (RecurseB <$> D.value)

derive instance Generic TestRecursiveB _

derive instance Newtype TestRecursiveB _

--------------------------------------------------------------------------------

_RecurseB :: Iso' TestRecursiveB TestRecursiveB
_RecurseB = _Newtype

--------------------------------------------------------------------------------

newtype TestRecord a = TestRecord
  { _field1 :: Maybe Int
  , _field2 :: a
  }

derive instance Functor TestRecord

derive instance (Eq a) => Eq (TestRecord a)

instance (Show a) => Show (TestRecord a) where
  show a = genericShow a

derive instance (Ord a) => Ord (TestRecord a)

instance (EncodeJson a) => EncodeJson (TestRecord a) where
  encodeJson = defer \_ -> E.encode $ unwrap >$< (E.record
                                                   { _field1: (E.maybe E.value) :: _ (Maybe Int)
                                                   , _field2: E.value :: _ a
                                                   })

instance (DecodeJson a) => DecodeJson (TestRecord a) where
  decodeJson = defer \_ -> D.decode $ (TestRecord <$> D.record "TestRecord"
      { _field1: (D.maybe D.value) :: _ (Maybe Int)
      , _field2: D.value :: _ a
      })

derive instance Generic (TestRecord a) _

derive instance Newtype (TestRecord a) _

--------------------------------------------------------------------------------

_TestRecord :: forall a. Iso' (TestRecord a) {_field1 :: Maybe Int, _field2 :: a}
_TestRecord = _Newtype

field1 :: forall a. Lens' (TestRecord a) (Maybe Int)
field1 = _Newtype <<< prop (Proxy :: _"_field1")

field2 :: forall a. Lens' (TestRecord a) a
field2 = _Newtype <<< prop (Proxy :: _"_field2")

--------------------------------------------------------------------------------

newtype TestNewtype = TestNewtype (TestRecord Boolean)

derive instance Eq TestNewtype

instance Show TestNewtype where
  show a = genericShow a

derive instance Ord TestNewtype

instance EncodeJson TestNewtype where
  encodeJson = defer \_ -> E.encode $ unwrap >$< E.value

instance DecodeJson TestNewtype where
  decodeJson = defer \_ -> D.decode $ (TestNewtype <$> D.value)

derive instance Generic TestNewtype _

derive instance Newtype TestNewtype _

--------------------------------------------------------------------------------

_TestNewtype :: Iso' TestNewtype (TestRecord Boolean)
_TestNewtype = _Newtype

--------------------------------------------------------------------------------

newtype TestNewtypeRecord = TestNewtypeRecord { unTestNewtypeRecord :: TestNewtype }

derive instance Eq TestNewtypeRecord

instance Show TestNewtypeRecord where
  show a = genericShow a

derive instance Ord TestNewtypeRecord

instance EncodeJson TestNewtypeRecord where
  encodeJson = defer \_ -> E.encode $ unwrap >$< (E.record
                                                 { unTestNewtypeRecord: E.value :: _ TestNewtype })

instance DecodeJson TestNewtypeRecord where
  decodeJson = defer \_ -> D.decode $ (TestNewtypeRecord <$> D.record "TestNewtypeRecord" { unTestNewtypeRecord: D.value :: _ TestNewtype })

derive instance Generic TestNewtypeRecord _

derive instance Newtype TestNewtypeRecord _

--------------------------------------------------------------------------------

_TestNewtypeRecord :: Iso' TestNewtypeRecord {unTestNewtypeRecord :: TestNewtype}
_TestNewtypeRecord = _Newtype

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

instance Show TestMultiInlineRecords where
  show a = genericShow a

derive instance Ord TestMultiInlineRecords

instance EncodeJson TestMultiInlineRecords where
  encodeJson = defer \_ -> case _ of
    Foo {_foo1, _foo2} -> encodeJson
      { tag: "Foo"
      , _foo1: flip E.encode _foo1 (E.maybe E.value)
      , _foo2: flip E.encode _foo2 E.unit
      }
    Bar {_bar1, _bar2} -> encodeJson
      { tag: "Bar"
      , _bar1: flip E.encode _bar1 E.value
      , _bar2: flip E.encode _bar2 E.value
      }

instance DecodeJson TestMultiInlineRecords where
  decodeJson = defer \_ -> D.decode
    $ D.sumType "TestMultiInlineRecords" $ Map.fromFoldable
      [ "Foo" /\ (Foo <$> D.object "Foo"
        { _foo1: (D.maybe D.value) :: _ (Maybe Int)
        , _foo2: D.unit :: _ Unit
        })
      , "Bar" /\ (Bar <$> D.object "Bar"
        { _bar1: D.value :: _ String
        , _bar2: D.value :: _ Boolean
        })
      ]

derive instance Generic TestMultiInlineRecords _

--------------------------------------------------------------------------------

_Foo :: Prism' TestMultiInlineRecords {_foo1 :: Maybe Int, _foo2 :: Unit}
_Foo = prism' Foo case _ of
  (Foo a) -> Just a
  _ -> Nothing

_Bar :: Prism' TestMultiInlineRecords {_bar1 :: String, _bar2 :: Boolean}
_Bar = prism' Bar case _ of
  (Bar a) -> Just a
  _ -> Nothing

--------------------------------------------------------------------------------

data TestTwoFields = TestTwoFields Boolean Int

derive instance Eq TestTwoFields

instance Show TestTwoFields where
  show a = genericShow a

derive instance Ord TestTwoFields

instance EncodeJson TestTwoFields where
  encodeJson = defer \_ -> E.encode $ (case _ of TestTwoFields a b -> (a /\ b)) >$< (E.tuple (E.value >/\< E.value))

instance DecodeJson TestTwoFields where
  decodeJson = defer \_ -> D.decode $ (D.tuple $ TestTwoFields </$\>D.value </*\> D.value)

derive instance Generic TestTwoFields _

--------------------------------------------------------------------------------

_TestTwoFields :: Iso' TestTwoFields {a :: Boolean, b :: Int}
_TestTwoFields = iso (\(TestTwoFields a b) -> {a, b}) (\{a, b} -> (TestTwoFields a b))

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

instance Show TestEnum where
  show a = genericShow a

derive instance Ord TestEnum

instance EncodeJson TestEnum where
  encodeJson = defer \_ -> E.encode E.enum

instance DecodeJson TestEnum where
  decodeJson = defer \_ -> D.decode D.enum

derive instance Generic TestEnum _

instance Enum TestEnum where
  succ = genericSucc
  pred = genericPred

instance Bounded TestEnum where
  bottom = genericBottom
  top = genericTop

--------------------------------------------------------------------------------

_Mon :: Prism' TestEnum Unit
_Mon = prism' (const Mon) case _ of
  Mon -> Just unit
  _ -> Nothing

_Tue :: Prism' TestEnum Unit
_Tue = prism' (const Tue) case _ of
  Tue -> Just unit
  _ -> Nothing

_Wed :: Prism' TestEnum Unit
_Wed = prism' (const Wed) case _ of
  Wed -> Just unit
  _ -> Nothing

_Thu :: Prism' TestEnum Unit
_Thu = prism' (const Thu) case _ of
  Thu -> Just unit
  _ -> Nothing

_Fri :: Prism' TestEnum Unit
_Fri = prism' (const Fri) case _ of
  Fri -> Just unit
  _ -> Nothing

_Sat :: Prism' TestEnum Unit
_Sat = prism' (const Sat) case _ of
  Sat -> Just unit
  _ -> Nothing

_Sun :: Prism' TestEnum Unit
_Sun = prism' (const Sun) case _ of
  Sun -> Just unit
  _ -> Nothing

--------------------------------------------------------------------------------

data MyUnit = U

derive instance Eq MyUnit

instance Show MyUnit where
  show a = genericShow a

derive instance Ord MyUnit

instance EncodeJson MyUnit where
  encodeJson = defer \_ -> E.encode E.enum

instance DecodeJson MyUnit where
  decodeJson = defer \_ -> D.decode D.enum

derive instance Generic MyUnit _

instance Enum MyUnit where
  succ = genericSucc
  pred = genericPred

instance Bounded MyUnit where
  bottom = genericBottom
  top = genericTop

--------------------------------------------------------------------------------

_U :: Iso' MyUnit Unit
_U = iso (const unit) (const U)
