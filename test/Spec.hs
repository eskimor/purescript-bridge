{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Main where

import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as Map
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import           Data.Word (Word, Word64)
import           Language.PureScript.Bridge
import           Language.PureScript.Bridge.SumType (equal1)
import           Language.PureScript.Bridge.TypeParameters
import           Test.Hspec (Spec, describe, hspec, it)
import           Test.Hspec.Expectations.Pretty
import           TestData
import           Text.PrettyPrint.Leijen.Text (Doc, pretty, text, vsep)

shouldRender :: Doc -> T.Text -> Expectation
shouldRender actual expected = renderText actual `shouldBe` T.stripEnd expected

main :: IO ()
main = hspec allTests

custom :: SumType 'Haskell -> SumType 'Haskell
custom (SumType t cs is) = SumType t cs $ customInstance : is
  where
    customInstance =
        Custom $
            CustomInstance [] (TypeInfo "" "Data.MyClass" "MyClass" [TypeInfo "" "" "Foo" []]) $
                Explicit
                    [ InstanceMember "member1" ["foo", "bar"] "undefined" [] mempty
                    , InstanceMember "member2" [] "do\npure unit" [] mempty
                    ]

customNewtypeDerived :: SumType 'Haskell -> SumType 'Haskell
customNewtypeDerived (SumType t cs is) = SumType t cs $ customInstance : is
  where
    customInstance =
        Custom $
            CustomInstance
                [TypeInfo "" "" "Eq" [TypeInfo "" "" "Foo" []]]
                (TypeInfo "" "Data.MyNTClass" "MyNTClass" [TypeInfo "" "" "Foo" []])
                DeriveNewtype

customDerived :: SumType 'Haskell -> SumType 'Haskell
customDerived (SumType t cs is) = SumType t cs $ customInstance : is
  where
    customInstance =
        Custom $
            CustomInstance
                [ TypeInfo "" "" "Eq" [TypeInfo "" "" "Foo" []]
                , TypeInfo "" "" "Show" [TypeInfo "" "" "Foo" []]
                ]
                (TypeInfo "" "Data.MyDClass" "MyDClass" [TypeInfo "" "" "Foo" []])
                Derive

allTests :: Spec
allTests = do
    describe "buildBridge without lens-code-gen" $ do
        it "tests generation of custom typeclasses" $
            let sumType =
                  bridgeSumType
                    (buildBridge defaultBridge)
                    (customNewtypeDerived . customDerived . custom $ mkSumType @Foo)
                doc = vsep $ sumTypeToDocs sumType
                txt :: T.Text = T.unlines
                    [ ""
                    , "data Foo"
                    , "  = Foo"
                    , "  | Bar Int"
                    , "  | FooBar Int String"
                    , ""
                    , "derive instance Generic Foo _"
                    , ""
                    , "derive newtype instance (Eq Foo) => MyNTClass Foo"
                    , ""
                    , "derive instance (Eq Foo, Show Foo) => MyDClass Foo"
                    , ""
                    , "instance MyClass Foo where"
                    , "  member1 foo bar = undefined"
                    , "  member2 = do"
                    , "    pure unit"
                    ]
             in doc `shouldRender` txt
        it "tests generation of typeclasses for custom type Foo" $
            let sumType =
                    bridgeSumType
                      (buildBridge defaultBridge)
                      (genericShow . order $ mkSumType @Foo)
                doc = vsep $ sumTypeToDocs sumType
                txt :: T.Text =
                  T.unlines
                    [ ""
                    , "data Foo"
                    ,  "  = Foo"
                    , "  | Bar Int"
                    , "  | FooBar Int String"
                    , ""
                    , "instance Show Foo where"
                    , "  show a = genericShow a"
                    , ""
                    , "derive instance Eq Foo"
                    , ""
                    , "derive instance Ord Foo"
                    , ""
                    , "derive instance Generic Foo _"
                    ]
             in doc `shouldRender` txt
        it "tests generation of typeclasses for custom type Func" $
            let sumType =
                  bridgeSumType
                    (buildBridge defaultBridge)
                    (equal1 . functor . genericShow $ mkSumType @(Func A))
                doc = vsep $ sumTypeToDocs sumType
                txt :: T.Text = T.unlines
                   [ ""
                   , "data Func a = Func Int a"
                   , ""
                   , "derive instance Eq1 Func"
                   , ""
                   , "instance (Show a) => Show (Func a) where"
                   , "  show a = genericShow a"
                   , ""
                   , "derive instance Functor Func"
                   , ""
                   , "derive instance Generic (Func a) _"
                   ]
            in doc `shouldRender` txt
        it "tests the generation of a whole (dummy) module" $
            let advanced' :: SumType 'PureScript
                advanced' = bridgeSumType
                    (buildBridge defaultBridge)
                    (mkSumType @(Bar A B M1 C))
                modules :: Modules
                modules = sumTypeToModule Nothing advanced'
                m = head . map moduleToText . Map.elems $ modules
                txt :: T.Text = T.unlines
                    [ "-- File auto generated by purescript-bridge! --"
                    , "module TestData where"
                    , ""
                    , "import Prelude"
                    , ""
                    , "import Data.Either (Either)"
                    , "import Data.Generic.Rep (class Generic)"
                    , "import Data.Maybe (Maybe(..))"
                    , ""
                    , ""
                    , "data Bar a b m c"
                    , "  = Bar1 (Maybe a)"
                    , "  | Bar2 (Either a b)"
                    , "  | Bar3 a"
                    , "  | Bar4 { myMonadicResult :: m b }"
                    , ""
                    , "derive instance Generic (Bar a b m c) _"
                    ]
             in m `shouldBe` txt
        it "tests generation of newtypes for record data type" $
            let recType' =
                  bridgeSumType
                    (buildBridge defaultBridge)
                    (newtypes $ mkSumType @(SingleRecord A B))
                doc = vsep $ sumTypeToDocs recType'
                txt :: T.Text = T.unlines
                    [ ""
                    , "newtype SingleRecord a b = SingleRecord"
                    , "  { _a :: a"
                    , "  , _b :: b"
                    , "  , c :: String"
                    , "  }"
                    , ""
                    , "derive instance Newtype (SingleRecord a b) _"
                    , ""
                    , "derive instance Generic (SingleRecord a b) _"
                    ]
            in doc `shouldRender` txt
        it "tests generation of newtypes for haskell newtype" $
          let recType' = bridgeSumType
                  (buildBridge defaultBridge)
                  (newtypes $ mkSumType @SomeNewtype)
              doc = vsep $ sumTypeToDocs recType'
              txt :: T.Text = T.unlines
                  [ ""
                  , "newtype SomeNewtype = SomeNewtype Int"
                  , ""
                  , "derive instance Newtype SomeNewtype _"
                  , ""
                  , "derive instance Generic SomeNewtype _"
                  ]
           in doc `shouldRender` txt
        it "tests generation of newtypes for haskell data type with one argument" $
            let recType' = bridgeSumType
                    (buildBridge defaultBridge)
                    (newtypes $ mkSumType @SingleValueConstr)
                doc = vsep $ sumTypeToDocs recType'
                txt :: T.Text = T.unlines
                    [ ""
                    , "newtype SingleValueConstr = SingleValueConstr Int"
                    ,  ""
                    , "derive instance Newtype SingleValueConstr _"
                    , ""
                    , "derive instance Generic SingleValueConstr _"
                    ]
             in doc `shouldRender` txt
        it "tests generation for haskell data type with one constructor, two arguments" $
            let recType' = bridgeSumType
                      (buildBridge defaultBridge)
                      (mkSumType @SingleProduct)
                doc = vsep $ sumTypeToDocs recType'
                txt :: T.Text = T.unlines
                    [ ""
                    , "data SingleProduct = SingleProduct String Int"
                    , ""
                    , "derive instance Generic SingleProduct _"
                    ]
            in doc `shouldRender` txt
        it "tests generation Eq instances for polymorphic types" $
            let recType' = bridgeSumType
                    (buildBridge defaultBridge)
                    (newtypes . equal $ mkSumType @(SingleRecord A B))
                doc = vsep $ sumTypeToDocs recType'
                txt :: T.Text = T.unlines
                    [ ""
                    , "newtype SingleRecord a b = SingleRecord"
                    , "  { _a :: a"
                    , "  , _b :: b"
                    , "  , c :: String"
                    , "  }"
                    , ""
                    , "derive instance (Eq a, Eq b) => Eq (SingleRecord a b)"
                    , ""
                    , "derive instance Newtype (SingleRecord a b) _"
                    , ""
                    , "derive instance Generic (SingleRecord a b) _"
                    ]
            in doc `shouldRender` txt
        it "tests generation of Ord instances for polymorphic types" $
            let recType' = bridgeSumType
                    (buildBridge defaultBridge)
                    (newtypes . order $ mkSumType @(SingleRecord A B))
                doc = vsep $ sumTypeToDocs recType'
                txt :: T.Text = T.unlines
                    [ ""
                    , "newtype SingleRecord a b = SingleRecord"
                    , "  { _a :: a"
                    , "  , _b :: b"
                    , "  , c :: String"
                    , "  }"
                    , ""
                    , "derive instance (Eq a, Eq b) => Eq (SingleRecord a b)"
                    , ""
                    , "derive instance (Ord a, Ord b) => Ord (SingleRecord a b)"
                    , ""
                    , "derive instance Newtype (SingleRecord a b) _"
                    , ""
                    , "derive instance Generic (SingleRecord a b) _"
                    ]
            in doc `shouldRender` txt

    describe "buildBridge for purescript 0.15" $ do
        it "tests with Int" $
            let bst = buildBridge defaultBridge (mkTypeInfo @Int)
                ti =
                    TypeInfo
                        { _typePackage = ""
                        , _typeModule = "Prim"
                        , _typeName = "Int"
                        , _typeParameters = []
                        }
             in bst `shouldBe` ti
        it "tests with custom type Foo" $
            let bst = bridgeSumType (buildBridge defaultBridge) (order (mkSumType @Foo))
                st =
                    SumType
                        TypeInfo {_typePackage = "", _typeModule = "TestData", _typeName = "Foo", _typeParameters = []}
                        [ DataConstructor {_sigConstructor = "Foo", _sigValues = Nullary}
                        , DataConstructor
                            { _sigConstructor = "Bar"
                            , _sigValues = Normal $ NEL.fromList
                              [ TypeInfo {_typePackage = ""
                                         , _typeModule = "Prim"
                                         , _typeName = "Int"
                                         , _typeParameters = []
                                         }
                              ]
                            }
                        , DataConstructor
                            { _sigConstructor = "FooBar"
                            , _sigValues =
                                Normal $ NEL.fromList
                                    [ TypeInfo { _typePackage = ""
                                               , _typeModule = "Prim"
                                               , _typeName = "Int"
                                               , _typeParameters = []
                                               }
                                    , TypeInfo { _typePackage = ""
                                               , _typeModule = "Prim"
                                               , _typeName = "String"
                                               , _typeParameters = []
                                               }
                                    ]
                            }
                        ]
                        [Eq, Ord, Maybe, Generic]
             in bst `shouldBe` st
        it "tests generation of for custom type Foo" $
            let recType = bridgeSumType (buildBridge defaultBridge) (prisms . argonautAesonGeneric . order  $ mkSumType @Foo)
                recTypeText :: Doc = vsep $ sumTypeToDocs recType
                txt :: T.Text = T.unlines
                    [ ""
                    , "data Foo"
                    , "  = Foo"
                    , "  | Bar Int"
                    , "  | FooBar Int String"
                    , ""
                    , "derive instance Eq Foo"
                    , ""
                    , "derive instance Ord Foo"
                    , ""
                    , "derive instance Generic Foo _"
                    , ""
                    , "instance EncodeJson Foo where"
                    , "  encodeJson = genericEncodeAeson Argonaut.defaultOptions"
                    , ""
                    , "instance DecodeJson Foo where"
                    , "  decodeJson = genericDecodeAeson Argonaut.defaultOptions"
                    , ""
                    , ""
                    , ""
                    , "_Foo :: Prism' Foo Unit"
                    , "_Foo = prism' (const Foo) case _ of"
                    , "  Foo -> Just unit"
                    , "  _ -> Nothing"
                    , "_Bar :: Prism' Foo Int"
                    , "_Bar = prism' Bar case _ of"
                    , "  (Bar a) -> Just a"
                    , "  _ -> Nothing"
                    , "_FooBar :: Prism' Foo {a :: Int, b :: String}"
                    , "_FooBar = prism' (\\{a, b} -> (FooBar a b)) case _ of"
                    , "  (FooBar a b) -> Just {a, b}"
                    , "  _ -> Nothing"
                    ]
             in recTypeText `shouldRender` txt
        it "tests the generation of a whole (dummy) module" $
            let advanced = bridgeSumType
                  (buildBridge defaultBridge)
                  (prisms . argonautAesonGeneric $ mkSumType @(Bar A B M1 C))
                modules = sumTypeToModule Nothing advanced
                m = head . map moduleToText . Map.elems $ modules
                txt :: T.Text = T.unlines
                    [ "-- File auto generated by purescript-bridge! --"
                    , "module TestData where"
                    , ""
                    , "import Prelude"
                    , ""
                    , "import Data.Argonaut.Aeson.Decode.Generic (genericDecodeAeson)"
                    , "import Data.Argonaut.Aeson.Encode.Generic (genericEncodeAeson)"
                    , "import Data.Argonaut.Aeson.Options as Argonaut"
                    , "import Data.Argonaut.Decode.Class (class DecodeJson, class DecodeJsonField, decodeJson)"
                    , "import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)"
                    , "import Data.Either (Either)"
                    , "import Data.Generic.Rep (class Generic)"
                    , "import Data.Lens (Iso', Lens', Prism', iso, lens, prism')"
                    , "import Data.Lens.Iso.Newtype (_Newtype)"
                    , "import Data.Lens.Record (prop)"
                    , "import Data.Maybe (Maybe(..))"
                    , "import Type.Proxy (Proxy(Proxy))"
                    , ""
                    , ""
                    , "data Bar a b m c"
                    , "  = Bar1 (Maybe a)"
                    , "  | Bar2 (Either a b)"
                    , "  | Bar3 a"
                    , "  | Bar4 { myMonadicResult :: m b }"
                    , ""
                    , "derive instance (Generic a ra, Generic b rb, Generic (m b) rmb) => Generic (Bar a b m c) _"
                    , ""
                    , "instance (Generic a ra, EncodeJson a, Generic b rb, EncodeJson b, Generic (m b) rmb, EncodeJson (m b)) => EncodeJson (Bar a b m c) where"
                    , "  encodeJson = genericEncodeAeson Argonaut.defaultOptions"
                    , ""
                    , "instance (Generic a ra, DecodeJson a, DecodeJsonField a, Generic b rb, DecodeJson b, DecodeJsonField b, Generic (m b) rmb, DecodeJson (m b), DecodeJsonField (m b)) => DecodeJson (Bar a b m c) where"
                    , "  decodeJson = genericDecodeAeson Argonaut.defaultOptions"
                    , "--------------------------------------------------------------------------------"
                    , ""
                    , ""
                    , ""
                    , ""
                    , "_Bar1 :: forall a b m c. Prism' (Bar a b m c) (Maybe a)"
                    , "_Bar1 = prism' Bar1 case _ of"
                    , "  (Bar1 a) -> Just a"
                    , "  _ -> Nothing"
                    , "_Bar2 :: forall a b m c. Prism' (Bar a b m c) (Either a b)"
                    , "_Bar2 = prism' Bar2 case _ of"
                    , "  (Bar2 a) -> Just a"
                    , "  _ -> Nothing"
                    , "_Bar3 :: forall a b m c. Prism' (Bar a b m c) a"
                    , "_Bar3 = prism' Bar3 case _ of"
                    , "  (Bar3 a) -> Just a"
                    , "  _ -> Nothing"
                    , "_Bar4 :: forall a b m c. Prism' (Bar a b m c) {myMonadicResult :: m b}"
                    , "_Bar4 = prism' Bar4 case _ of"
                    , "  (Bar4 a) -> Just a"
                    , "  _ -> Nothing"
                    ]
             in m `shouldBe` txt
        it "tests generation of record optics" $
            let recType = bridgeSumType (buildBridge defaultBridge) (lenses $ mkSumType @(SingleRecord A B))
                bar = bridgeSumType (buildBridge defaultBridge) (mkSumType @(Bar A B M1 C))
                barOptics = vsep $ recordOptics bar
                recTypeOptics = vsep $ recordOptics recType
                txt :: T.Text = T.unlines
                    [ "a :: forall a b. Lens' (SingleRecord a b) a"
                    , "a = _Newtype <<< prop (Proxy :: _\"_a\")"
                    , "b :: forall a b. Lens' (SingleRecord a b) b"
                    , "b = _Newtype <<< prop (Proxy :: _\"_b\")"
                    , ""
                    ]
             in (barOptics <> recTypeOptics) `shouldRender` txt
        it "tests generation of newtypes for record data type" $
            let recType = bridgeSumType (buildBridge defaultBridge) (lenses . argonautAesonGeneric $ mkSumType @(SingleRecord A B))
                recTypeText = vsep $ sumTypeToDocs recType
                txt :: T.Text = T.unlines
                    [ ""
                    , "newtype SingleRecord a b = SingleRecord"
                    , "  { _a :: a"
                    , "  , _b :: b"
                    , "  , c :: String"
                    , "  }"
                    , ""
                    , "derive instance Newtype (SingleRecord a b) _"
                    , ""
                    , "derive instance (Generic a ra, Generic b rb) => Generic (SingleRecord a b) _"
                    , ""
                    , "instance (Generic a ra, EncodeJson a, Generic b rb, EncodeJson b) => EncodeJson (SingleRecord a b) where"
                    , "  encodeJson = genericEncodeAeson Argonaut.defaultOptions"
                    , ""
                    , "instance (Generic a ra, DecodeJson a, DecodeJsonField a, Generic b rb, DecodeJson b, DecodeJsonField b) => DecodeJson (SingleRecord a b) where"
                    , "  decodeJson = genericDecodeAeson Argonaut.defaultOptions"
                    , ""
                    , ""
                    , ""
                    , "_SingleRecord :: forall a b. Iso' (SingleRecord a b) {_a :: a, _b :: b, c :: String}"
                    , "_SingleRecord = _Newtype"
                    , "a :: forall a b. Lens' (SingleRecord a b) a"
                    , "a = _Newtype <<< prop (Proxy :: _\"_a\")"
                    , "b :: forall a b. Lens' (SingleRecord a b) b"
                    , "b = _Newtype <<< prop (Proxy :: _\"_b\")"
                    ]
             in recTypeText `shouldRender` txt
        it "tests generation of newtypes for haskell newtype" $
            let recType = bridgeSumType (buildBridge defaultBridge) (prisms . argonautAesonGeneric $ mkSumType @SomeNewtype)
                recTypeText = vsep $ sumTypeToDocs recType
                txt :: T.Text = T.unlines
                    [ ""
                    , "newtype SomeNewtype = SomeNewtype Int"
                    , ""
                    , "derive instance Newtype SomeNewtype _"
                    , ""
                    , "derive instance Generic SomeNewtype _"
                    , ""
                    , "instance EncodeJson SomeNewtype where"
                    , "  encodeJson = genericEncodeAeson Argonaut.defaultOptions"
                    , ""
                    , "instance DecodeJson SomeNewtype where"
                    , "  decodeJson = genericDecodeAeson Argonaut.defaultOptions"
                    , ""
                    , ""
                    , ""
                    , "_SomeNewtype :: Iso' SomeNewtype Int"
                    , "_SomeNewtype = _Newtype"
                    ]
             in recTypeText `shouldRender` txt
        it "tests generation of newtypes for haskell data type with one argument" $
            let recType = bridgeSumType (buildBridge defaultBridge) (lenses . argonautAesonGeneric $ mkSumType @SingleValueConstr)
                recTypeText = vsep $ sumTypeToDocs recType
                txt :: T.Text = T.unlines
                    [ ""
                    , "newtype SingleValueConstr = SingleValueConstr Int"
                    , ""
                    , "derive instance Newtype SingleValueConstr _"
                    , ""
                    , "derive instance Generic SingleValueConstr _"
                    , ""
                    , "instance EncodeJson SingleValueConstr where"
                    , "  encodeJson = genericEncodeAeson Argonaut.defaultOptions"
                  , ""
                    , "instance DecodeJson SingleValueConstr where"
                    , "  decodeJson = genericDecodeAeson Argonaut.defaultOptions"
                    , ""
                    , ""
                    , ""
                    , "_SingleValueConstr :: Iso' SingleValueConstr Int"
                    , "_SingleValueConstr = _Newtype"
                    ]
             in recTypeText `shouldRender` txt
        it "tests generation for haskell data type with one constructor, two arguments" $
            let recType = bridgeSumType (buildBridge defaultBridge) (lenses . argonautAesonGeneric $ mkSumType @SingleProduct)
                recTypeText = vsep $ sumTypeToDocs recType
                txt :: T.Text = T.unlines
                  [ ""
                  , "data SingleProduct = SingleProduct String Int"
                  , ""
                  , "derive instance Generic SingleProduct _"
                  , ""
                  , "instance EncodeJson SingleProduct where"
                  , "  encodeJson = genericEncodeAeson Argonaut.defaultOptions"
                  , ""
                  , "instance DecodeJson SingleProduct where"
                  , "  decodeJson = genericDecodeAeson Argonaut.defaultOptions"
                  , ""
                  , ""
                  , ""
                  , "_SingleProduct :: Iso' SingleProduct {a :: String, b :: Int}"
                  , "_SingleProduct = iso (\\(SingleProduct a b) -> {a, b}) (\\{a, b} -> (SingleProduct a b))"
                  ]
             in recTypeText `shouldRender` txt
        it "tests that sum types with multiple constructors don't generate record optics" $
            let recType = bridgeSumType (buildBridge defaultBridge) (mkSumType @TwoRecords)
                recTypeOptics :: Doc = vsep $ recordOptics recType
                txt :: T.Text = ""
             in recTypeOptics `shouldRender` txt -- No record optics for multi-constructors
    describe "buildBridge without lens-code-gen and argonaut-codecs" $ do
        it "tests generation of for custom type Foo" $
            let recType = bridgeSumType (buildBridge defaultBridge) (order (mkSumType @Foo))
                recTypeText = vsep $ sumTypeToDocs recType
                txt :: T.Text = T.unlines
                    [ ""
                    , "data Foo"
                    , "  = Foo"
                    , "  | Bar Int"
                    , "  | FooBar Int String"
                    , ""
                    , "derive instance Eq Foo"
                    , ""
                    , "derive instance Ord Foo"
                    , ""
                    , "derive instance Generic Foo _"
                    ]
             in recTypeText `shouldRender` txt
        it "tests the generation of a whole (dummy) module" $
            let advanced' = bridgeSumType (buildBridge defaultBridge) (mkSumType @(Bar A B M1 C))
                modules = sumTypeToModule Nothing advanced'
                m = head . map moduleToText . Map.elems $ modules
                txt :: T.Text = T.unlines
                    [ "-- File auto generated by purescript-bridge! --"
                    , "module TestData where"
                    , ""
                    , "import Prelude"
                    , ""
                    , "import Data.Either (Either)"
                    , "import Data.Generic.Rep (class Generic)"
                    , "import Data.Maybe (Maybe(..))"
                    , ""
                    , ""
                    , "data Bar a b m c"
                    , "  = Bar1 (Maybe a)"
                    , "  | Bar2 (Either a b)"
                    , "  | Bar3 a"
                    , "  | Bar4 { myMonadicResult :: m b }"
                    , ""
                    , "derive instance (Generic a, Generic b, Generic (m b)) => Generic (Bar a b m c)"
                    ]
             in m `shouldBe` txt
        it "tests generation of newtypes for record data type" $
            let recType' = bridgeSumType (buildBridge defaultBridge) (mkSumType @(SingleRecord A B))
                recTypeText = vsep $ sumTypeToDocs recType'
                txt :: T.Text = T.unlines
                    [ ""
                    , "newtype SingleRecord a b = SingleRecord"
                    , "  { _a :: a"
                    , "  , _b :: b"
                    , "  , c :: String"
                    , "  }"
                    , ""
                    , "derive instance Newtype (SingleRecord a b) _"
                    , ""
                    , "derive instance (Generic a, Generic b) => Generic (SingleRecord a b)"
                    ]
             in recTypeText `shouldRender` txt
        it "tests generation of newtypes for haskell newtype" $
            let recType' = bridgeSumType (buildBridge defaultBridge) (mkSumType @SomeNewtype)
                recTypeText = vsep $ sumTypeToDocs recType'
                txt :: T.Text = T.unlines
                    [ ""
                    , "newtype SomeNewtype = SomeNewtype Int"
                    , ""
                    , "derive instance Newtype SomeNewtype _"
                    , ""
                    , "derive instance Generic SomeNewtype _"
                    ]
             in recTypeText `shouldRender` txt
        it "tests generation of newtypes for haskell data type with one argument" $
            let recType' = bridgeSumType (buildBridge defaultBridge) (mkSumType @SingleValueConstr)
                recTypeText = vsep $ sumTypeToDocs recType'
                txt :: T.Text = T.unlines
                    [ ""
                    , "newtype SingleValueConstr = SingleValueConstr Int"
                    , ""
                    , "derive instance Newtype SingleValueConstr _"
                    , ""
                    , "derive instance Generic SingleValueConstr _"
                    ]
             in recTypeText `shouldRender` txt
        it "tests generation for haskell data type with one constructor, two arguments" $
            let recType' = bridgeSumType (buildBridge defaultBridge) (mkSumType @SingleProduct)
                recTypeText = vsep $ sumTypeToDocs recType'
                txt :: T.Text = T.unlines
                    [ ""
                    , "data SingleProduct = SingleProduct String Int"
                    , ""
                    , "derive instance Generic SingleProduct _"
                    ]
             in recTypeText `shouldRender` txt

    describe "buildBridge without lens-code-gen, generics-rep, and argonaut-codecs" $ do
        it "tests generation of for custom type Foo" $
            let recType =
                  bridgeSumType
                    (buildBridge defaultBridge)
                    (genericShow . order $ mkSumType @Foo)
                recTypeText = vsep $ sumTypeToDocs recType
                txt :: T.Text = T.unlines
                    [ ""
                    , "data Foo"
                    , "  = Foo"
                    , "  | Bar Int"
                    , "  | FooBar Int String"
                    , ""
                    , "instance Show Foo where"
                    , "  show a = genericShow a"
                    , ""
                    , "derive instance Eq Foo"
                    , ""
                    , "derive instance Ord Foo"
                    , ""
                    , "derive instance Generic Foo _"
                    ]
             in recTypeText `shouldRender` txt
        it "tests the generation of a whole (dummy) module" $
            let advanced' = bridgeSumType (buildBridge defaultBridge) (mkSumType @(Bar A B M1 C))
                modules = sumTypeToModule Nothing advanced'
                m = head . map moduleToText . Map.elems $ modules
                txt :: T.Text = T.unlines
                    [ "-- File auto generated by purescript-bridge! --"
                    , "module TestData where"
                    , ""
                    , "import Prelude"
                    , ""
                    , "import Data.Either (Either)"
                    , "import Data.Generic.Rep (class Generic)"
                    , "import Data.Maybe (Maybe(..))"
                    , ""
                    , ""
                    , "data Bar a b m c"
                    , "  = Bar1 (Maybe a)"
                    , "  | Bar2 (Either a b)"
                    , "  | Bar3 a"
                    , "  | Bar4 { myMonadicResult :: m b }"
                    , ""
                    , "derive instance (Generic a ra, Generic b rb, Generic (m b) rmb) => Generic (Bar a b m c) _"
                    ]
             in m `shouldBe` txt
        it "tests generation of newtypes for record data type" $
            let recType' = bridgeSumType (buildBridge defaultBridge) (mkSumType @(SingleRecord A B))
                recTypeText = vsep $ sumTypeToDocs recType'
                txt :: T.Text = T.unlines
                    [ ""
                    , "newtype SingleRecord a b = SingleRecord"
                    , "  { _a :: a"
                    , "  , _b :: b"
                    , "  , c :: String"
                    , "  }"
                    , ""
                    , "derive instance Newtype (SingleRecord a b) _"
                    , ""
                    , "derive instance (Generic a ra, Generic b rb) => Generic (SingleRecord a b) _"
                    ]
             in recTypeText `shouldRender` txt

    describe "tests bridging Haskells Data.Word to PureScripts Data.Word from purescript-word" $ do
        describe "moduleToText" $
            it "should contain the right import and datatype" $ do
                let createModuleText :: SumType 'Haskell -> T.Text
                    createModuleText sumType =
                        let bridge = buildBridge defaultBridge
                            modules = sumTypeToModule Nothing (bridgeSumType bridge sumType)
                         in head . map moduleToText . Map.elems $ modules
                    expectedText = T.unlines
                        [ "-- File auto generated by purescript-bridge! --"
                        , "module TestData where"
                        , ""
                        , "import Prelude"
                        , ""
                        , "import Data.Generic.Rep (class Generic)"
                        , "import Data.Maybe (Maybe(..))"
                        , "import Data.Newtype (class Newtype)"
                        , "import Data.Word (Word64)"
                        , ""
                        , ""
                        , "newtype Simple Word64 = Simple Word64"
                        , ""
                        , "derive instance Newtype (Simple Word64) _"
                        , ""
                        , "derive instance Generic Word64 => Generic (Simple Word64) _"
                        ]
                createModuleText (mkSumType @(Simple Word64)) `shouldBe` expectedText
        describe "buildBridge" $
            it "should create the correct type information for Word" $ do
                let bst = buildBridge defaultBridge (mkTypeInfo @Word)
                    ti =
                        TypeInfo
                            { _typePackage = "purescript-word"
                            , _typeModule = "Data.Word"
                            , _typeName = "Word"
                            , _typeParameters = []
                            }
                 in bst `shouldBe` ti

        it "test generation of constructor optics" $
            let bar = bridgeSumType (buildBridge defaultBridge) (prisms $ mkSumType @(Bar A B M1 C))
                foo = bridgeSumType (buildBridge defaultBridge) (prisms $ mkSumType @Foo)
                barOptics = vsep $ constructorOptics bar
                fooOptics = vsep $ constructorOptics foo
                txt :: T.Text =
                    T.stripEnd $ T.unlines
                        [ ""
                        , ""
                        , "_Bar1 :: forall a b m c. Prism' (Bar a b m c) (Maybe a)"
                        , "_Bar1 = prism' Bar1 case _ of"
                        , "  (Bar1 a) -> Just a"
                        , "  _ -> Nothing"
                        , "_Bar2 :: forall a b m c. Prism' (Bar a b m c) (Either a b)"
                        , "_Bar2 = prism' Bar2 case _ of"
                        , "  (Bar2 a) -> Just a"
                        , "  _ -> Nothing"
                        , "_Bar3 :: forall a b m c. Prism' (Bar a b m c) a"
                        , "_Bar3 = prism' Bar3 case _ of"
                        , "  (Bar3 a) -> Just a"
                        , "  _ -> Nothing"
                        , "_Bar4 :: forall a b m c. Prism' (Bar a b m c) {myMonadicResult :: m b}"
                        , "_Bar4 = prism' Bar4 case _ of"
                        , "  (Bar4 a) -> Just a"
                        , "  _ -> Nothing"
                        , ""
                        , "_Foo :: Prism' Foo Unit"
                        , "_Foo = prism' (const Foo) case _ of"
                        , "  Foo -> Just unit"
                        , "  _ -> Nothing"
                        , "_Bar :: Prism' Foo Int"
                        , "_Bar = prism' Bar case _ of"
                        , "  (Bar a) -> Just a"
                        , "  _ -> Nothing"
                        , "_FooBar :: Prism' Foo {a :: Int, b :: String}"
                        , "_FooBar = prism' (\\{a, b} -> (FooBar a b)) case _ of"
                        , "  (FooBar a b) -> Just {a, b}"
                        , "  _ -> Nothing"
                        ]
             in (barOptics <> fooOptics) `shouldRender` txt
        it "tests that sum types with multiple constructors don't generate record optics" $
            let recType = bridgeSumType (buildBridge defaultBridge) (mkSumType @TwoRecords)
                recTypeOptics = vsep $ recordOptics recType
             in recTypeOptics `shouldRender` ("" :: T.Text) -- No record optics for multi-constructors
    describe "buildBridge without lens-code-gen and argonaut-codecs" $ do
        it "tests the generation of a whole (dummy) module" $
            let advanced' = bridgeSumType (buildBridge defaultBridge) (mkSumType @(Bar A B M1 C))
                modules = sumTypeToModule Nothing advanced'
                m = head . map moduleToText . Map.elems $ modules
                txt :: T.Text = T.unlines
                    [ "-- File auto generated by purescript-bridge! --"
                    , "module TestData where"
                    , ""
                    , "import Prelude"
                    , ""
                    , "import Data.Either (Either)"
                    , "import Data.Generic.Rep (class Generic)"
                    , "import Data.Maybe (Maybe(..))"
                    , ""
                    , ""
                    , "data Bar a b m c"
                    , "  = Bar1 (Maybe a)"
                    , "  | Bar2 (Either a b)"
                    , "  | Bar3 a"
                    , "  | Bar4 { myMonadicResult :: m b }"
                    , ""
                    , "derive instance (Generic a, Generic b, Generic (m b)) => Generic (Bar a b m c) _"
                    ]
             in m `shouldBe` txt
