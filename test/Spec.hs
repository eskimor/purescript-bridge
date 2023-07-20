{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Main where

import qualified Data.Map as Map
import           Data.Monoid ((<>))
import           Data.Proxy
import qualified Data.Text as T
import           Data.Word (Word, Word64)
import           Language.PureScript.Bridge
import           Language.PureScript.Bridge.CodeGenSwitches
import           Language.PureScript.Bridge.TypeParameters
import           Test.Hspec (Spec, describe, hspec, it)
import           Test.Hspec.Expectations.Pretty
import           TestData

main :: IO ()
main = hspec allTests

allTests :: Spec
allTests = do
    describe "buildBridge for purescript 0.14" $ do
        it "tests with Int" $
            let bst = buildBridge defaultBridge (mkTypeInfo (Proxy :: Proxy Int))
                ti =
                    TypeInfo
                        { _typePackage = ""
                        , _typeModule = "Prim"
                        , _typeName = "Int"
                        , _typeParameters = []
                        }
             in bst `shouldBe` ti
        it "tests with custom type Foo" $
            let prox = Proxy :: Proxy Foo
                bst = bridgeSumType (buildBridge defaultBridge) (showing prox . order prox $ mkSumType prox)
                st =
                    SumType
                        TypeInfo {_typePackage = "", _typeModule = "TestData", _typeName = "Foo", _typeParameters = []}
                        [ DataConstructor {_sigConstructor = "Foo", _sigValues = Left []}
                        , DataConstructor
                            { _sigConstructor = "Bar"
                            , _sigValues = Left [TypeInfo {_typePackage = "", _typeModule = "Prim", _typeName = "Int", _typeParameters = []}]
                            }
                        , DataConstructor
                            { _sigConstructor = "FooBar"
                            , _sigValues =
                                Left
                                    [ TypeInfo {_typePackage = "", _typeModule = "Prim", _typeName = "Int", _typeParameters = []}
                                    , TypeInfo {_typePackage = "", _typeModule = "Prim", _typeName = "String", _typeParameters = []}
                                    ]
                            }
                        ]
                        [Show, Eq, Ord, Encode, Decode, EncodeJson, DecodeJson, Generic]
             in bst `shouldBe` st
        it "tests generation of for custom type Foo" $
            let prox = Proxy :: Proxy Foo
                recType = bridgeSumType (buildBridge defaultBridge) (showing prox . order prox $ mkSumType prox)
                recTypeText = sumTypeToText defaultSettings recType
                txt =
                    T.stripEnd $
                        T.unlines
                            [ "data Foo ="
                            , "    Foo"
                            , "  | Bar Int"
                            , "  | FooBar Int String"
                            , ""
                            , "instance showFoo ∷ Show Foo where"
                            , "  show value = genericShow value"
                            , ""
                            , "derive instance eqFoo :: Eq Foo"
                            , "derive instance ordFoo :: Ord Foo"
                            , "instance encodeJsonFoo :: EncodeJson Foo where"
                            , "  encodeJson = genericEncodeAeson Argonaut.defaultOptions"
                            , "instance decodeJsonFoo :: DecodeJson Foo where"
                            , "  decodeJson = genericDecodeAeson Argonaut.defaultOptions"
                            , "derive instance genericFoo :: Generic Foo _"
                            , ""
                            , "--------------------------------------------------------------------------------"
                            , "_Foo :: Prism' Foo Unit"
                            , "_Foo = prism' (\\_ -> Foo) f"
                            , "  where"
                            , "    f Foo = Just unit"
                            , "    f _ = Nothing"
                            , ""
                            , "_Bar :: Prism' Foo Int"
                            , "_Bar = prism' Bar f"
                            , "  where"
                            , "    f (Bar a) = Just $ a"
                            , "    f _ = Nothing"
                            , ""
                            , "_FooBar :: Prism' Foo { a :: Int, b :: String }"
                            , "_FooBar = prism' (\\{ a, b } -> FooBar a b) f"
                            , "  where"
                            , "    f (FooBar a b) = Just $ { a: a, b: b }"
                            , "    f _ = Nothing"
                            , ""
                            , "--------------------------------------------------------------------------------"
                            ]
             in recTypeText `shouldBe` txt
        it "tests the generation of a whole (dummy) module" $
            let advanced = bridgeSumType (buildBridge defaultBridge) (mkSumType (Proxy :: Proxy (Bar A B M1 C)))
                modules = sumTypeToModule advanced Map.empty
                m = head . map (moduleToText defaultSettings) . Map.elems $ modules
                txt =
                    T.unlines
                        [ "-- File auto generated by purescript-bridge! --"
                        , "module TestData where"
                        , ""
                        , "import Data.Argonaut.Aeson.Decode.Generic (genericDecodeAeson)"
                        , "import Data.Argonaut.Aeson.Encode.Generic (genericEncodeAeson)"
                        , "import Data.Argonaut.Aeson.Options as Argonaut"
                        , "import Data.Argonaut.Decode.Class (class DecodeJson, class DecodeJsonField, decodeJson)"
                        , "import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)"
                        , "import Data.Either (Either)"
                        , "import Data.Generic.Rep (class Generic)"
                        , "import Data.Lens (Iso', Lens', Prism', lens, prism')"
                        , "import Data.Lens.Iso.Newtype (_Newtype)"
                        , "import Data.Lens.Record (prop)"
                        , "import Data.Maybe (Maybe, Maybe(..))"
                        , "import Data.Newtype (class Newtype)"
                        , "import Type.Proxy (Proxy(Proxy))"
                        , ""
                        , "import Prelude"
                        , ""
                        , "data Bar a b m c ="
                        , "    Bar1 (Maybe a)"
                        , "  | Bar2 (Either a b)"
                        , "  | Bar3 a"
                        , "  | Bar4 {"
                        , "      myMonadicResult :: m b"
                        , "    }"
                        , ""
                        , "instance encodeJsonBar :: (Generic a ra, EncodeJson a, Generic b rb, EncodeJson b, Generic (m b) rmb, EncodeJson (m b)) => EncodeJson (Bar a b m c) where"
                        , "  encodeJson = genericEncodeAeson Argonaut.defaultOptions"
                        , "instance decodeJsonBar :: (Generic a ra, DecodeJson a, DecodeJsonField a, Generic b rb, DecodeJson b, DecodeJsonField b, Generic (m b) rmb, DecodeJson (m b), DecodeJsonField (m b)) => DecodeJson (Bar a b m c) where"
                        , "  decodeJson = genericDecodeAeson Argonaut.defaultOptions"
                        , "derive instance genericBar :: (Generic a ra, Generic b rb, Generic (m b) rmb) => Generic (Bar a b m c) _"
                        , ""
                        , "--------------------------------------------------------------------------------"
                        , "_Bar1 :: forall a b m c. Prism' (Bar a b m c) (Maybe a)"
                        , "_Bar1 = prism' Bar1 f"
                        , "  where"
                        , "    f (Bar1 a) = Just $ a"
                        , "    f _ = Nothing"
                        , ""
                        , "_Bar2 :: forall a b m c. Prism' (Bar a b m c) (Either a b)"
                        , "_Bar2 = prism' Bar2 f"
                        , "  where"
                        , "    f (Bar2 a) = Just $ a"
                        , "    f _ = Nothing"
                        , ""
                        , "_Bar3 :: forall a b m c. Prism' (Bar a b m c) a"
                        , "_Bar3 = prism' Bar3 f"
                        , "  where"
                        , "    f (Bar3 a) = Just $ a"
                        , "    f _ = Nothing"
                        , ""
                        , "_Bar4 :: forall a b m c. Prism' (Bar a b m c) { myMonadicResult :: m b }"
                        , "_Bar4 = prism' Bar4 f"
                        , "  where"
                        , "    f (Bar4 r) = Just r"
                        , "    f _ = Nothing"
                        , ""
                        , "--------------------------------------------------------------------------------"
                        ]
             in m `shouldBe` txt
        it "test generation of constructor optics" $
            let bar = bridgeSumType (buildBridge defaultBridge) (mkSumType (Proxy :: Proxy (Bar A B M1 C)))
                foo = bridgeSumType (buildBridge defaultBridge) (mkSumType (Proxy :: Proxy Foo))
                barOptics = constructorOptics bar
                fooOptics = constructorOptics foo
                txt =
                    T.unlines
                        [ "_Bar1 :: forall a b m c. Prism' (Bar a b m c) (Maybe a)"
                        , "_Bar1 = prism' Bar1 f"
                        , "  where"
                        , "    f (Bar1 a) = Just $ a"
                        , "    f _ = Nothing"
                        , ""
                        , "_Bar2 :: forall a b m c. Prism' (Bar a b m c) (Either a b)"
                        , "_Bar2 = prism' Bar2 f"
                        , "  where"
                        , "    f (Bar2 a) = Just $ a"
                        , "    f _ = Nothing"
                        , ""
                        , "_Bar3 :: forall a b m c. Prism' (Bar a b m c) a"
                        , "_Bar3 = prism' Bar3 f"
                        , "  where"
                        , "    f (Bar3 a) = Just $ a"
                        , "    f _ = Nothing"
                        , ""
                        , "_Bar4 :: forall a b m c. Prism' (Bar a b m c) { myMonadicResult :: m b }"
                        , "_Bar4 = prism' Bar4 f"
                        , "  where"
                        , "    f (Bar4 r) = Just r"
                        , "    f _ = Nothing"
                        , ""
                        , "_Foo :: Prism' Foo Unit"
                        , "_Foo = prism' (\\_ -> Foo) f"
                        , "  where"
                        , "    f Foo = Just unit"
                        , "    f _ = Nothing"
                        , ""
                        , "_Bar :: Prism' Foo Int"
                        , "_Bar = prism' Bar f"
                        , "  where"
                        , "    f (Bar a) = Just $ a"
                        , "    f _ = Nothing"
                        , ""
                        , "_FooBar :: Prism' Foo { a :: Int, b :: String }"
                        , "_FooBar = prism' (\\{ a, b } -> FooBar a b) f"
                        , "  where"
                        , "    f (FooBar a b) = Just $ { a: a, b: b }"
                        , "    f _ = Nothing"
                        , ""
                        ]
             in (barOptics <> fooOptics) `shouldBe` txt
        it "tests generation of record optics" $
            let recType = bridgeSumType (buildBridge defaultBridge) (mkSumType (Proxy :: Proxy (SingleRecord A B)))
                bar = bridgeSumType (buildBridge defaultBridge) (mkSumType (Proxy :: Proxy (Bar A B M1 C)))
                barOptics = recordOptics bar
                recTypeOptics = recordOptics recType
                txt =
                    T.unlines
                        [ "a :: forall a b. Lens' (SingleRecord a b) a"
                        , "a = _Newtype <<< prop (Proxy :: Proxy \"_a\")"
                        , ""
                        , "b :: forall a b. Lens' (SingleRecord a b) b"
                        , "b = _Newtype <<< prop (Proxy :: Proxy \"_b\")"
                        , ""
                        ]
             in (barOptics <> recTypeOptics) `shouldBe` txt
        it "tests generation of newtypes for record data type" $
            let recType = bridgeSumType (buildBridge defaultBridge) (mkSumType (Proxy :: Proxy (SingleRecord A B)))
                recTypeText = sumTypeToText defaultSettings recType
                txt =
                    T.stripEnd $
                        T.unlines
                            [ "newtype SingleRecord a b ="
                            , "    SingleRecord {"
                            , "      _a :: a"
                            , "    , _b :: b"
                            , "    , c :: String"
                            , "    }"
                            , ""
                            , "instance encodeJsonSingleRecord :: (Generic a ra, EncodeJson a, Generic b rb, EncodeJson b) => EncodeJson (SingleRecord a b) where"
                            , "  encodeJson = genericEncodeAeson Argonaut.defaultOptions"
                            , "instance decodeJsonSingleRecord :: (Generic a ra, DecodeJson a, DecodeJsonField a, Generic b rb, DecodeJson b, DecodeJsonField b) => DecodeJson (SingleRecord a b) where"
                            , "  decodeJson = genericDecodeAeson Argonaut.defaultOptions"
                            , "derive instance genericSingleRecord :: (Generic a ra, Generic b rb) => Generic (SingleRecord a b) _"
                            , "derive instance newtypeSingleRecord :: Newtype (SingleRecord a b) _"
                            , ""
                            , "--------------------------------------------------------------------------------"
                            , "_SingleRecord :: forall a b. Iso' (SingleRecord a b) { _a :: a, _b :: b, c :: String}"
                            , "_SingleRecord = _Newtype"
                            , ""
                            , "a :: forall a b. Lens' (SingleRecord a b) a"
                            , "a = _Newtype <<< prop (Proxy :: Proxy \"_a\")"
                            , ""
                            , "b :: forall a b. Lens' (SingleRecord a b) b"
                            , "b = _Newtype <<< prop (Proxy :: Proxy \"_b\")"
                            , ""
                            , "--------------------------------------------------------------------------------"
                            ]
             in recTypeText `shouldBe` txt
        it "tests generation of newtypes for haskell newtype" $
            let recType = bridgeSumType (buildBridge defaultBridge) (mkSumType (Proxy :: Proxy SomeNewtype))
                recTypeText = sumTypeToText defaultSettings recType
                txt =
                    T.stripEnd $
                        T.unlines
                            [ "newtype SomeNewtype ="
                            , "    SomeNewtype Int"
                            , ""
                            , "instance encodeJsonSomeNewtype :: EncodeJson SomeNewtype where"
                            , "  encodeJson = genericEncodeAeson Argonaut.defaultOptions"
                            , "instance decodeJsonSomeNewtype :: DecodeJson SomeNewtype where"
                            , "  decodeJson = genericDecodeAeson Argonaut.defaultOptions"
                            , "derive instance genericSomeNewtype :: Generic SomeNewtype _"
                            , "derive instance newtypeSomeNewtype :: Newtype SomeNewtype _"
                            , ""
                            , "--------------------------------------------------------------------------------"
                            , "_SomeNewtype :: Iso' SomeNewtype Int"
                            , "_SomeNewtype = _Newtype"
                            , "--------------------------------------------------------------------------------"
                            ]
             in recTypeText `shouldBe` txt
        it "tests generation of newtypes for haskell data type with one argument" $
            let recType = bridgeSumType (buildBridge defaultBridge) (mkSumType (Proxy :: Proxy SingleValueConstr))
                recTypeText = sumTypeToText defaultSettings recType
                txt =
                    T.stripEnd $
                        T.unlines
                            [ "newtype SingleValueConstr ="
                            , "    SingleValueConstr Int"
                            , ""
                            , "instance encodeJsonSingleValueConstr :: EncodeJson SingleValueConstr where"
                            , "  encodeJson = genericEncodeAeson Argonaut.defaultOptions"
                            , "instance decodeJsonSingleValueConstr :: DecodeJson SingleValueConstr where"
                            , "  decodeJson = genericDecodeAeson Argonaut.defaultOptions"
                            , "derive instance genericSingleValueConstr :: Generic SingleValueConstr _"
                            , "derive instance newtypeSingleValueConstr :: Newtype SingleValueConstr _"
                            , ""
                            , "--------------------------------------------------------------------------------"
                            , "_SingleValueConstr :: Iso' SingleValueConstr Int"
                            , "_SingleValueConstr = _Newtype"
                            , "--------------------------------------------------------------------------------"
                            ]
             in recTypeText `shouldBe` txt
        it "tests generation for haskell data type with one constructor, two arguments" $
            let recType = bridgeSumType (buildBridge defaultBridge) (mkSumType (Proxy :: Proxy SingleProduct))
                recTypeText = sumTypeToText defaultSettings recType
                txt =
                    T.stripEnd $
                        T.unlines
                            [ "data SingleProduct ="
                            , "    SingleProduct String Int"
                            , ""
                            , "instance encodeJsonSingleProduct :: EncodeJson SingleProduct where"
                            , "  encodeJson = genericEncodeAeson Argonaut.defaultOptions"
                            , "instance decodeJsonSingleProduct :: DecodeJson SingleProduct where"
                            , "  decodeJson = genericDecodeAeson Argonaut.defaultOptions"
                            , "derive instance genericSingleProduct :: Generic SingleProduct _"
                            , ""
                            , "--------------------------------------------------------------------------------"
                            , "_SingleProduct :: Prism' SingleProduct { a :: String, b :: Int }"
                            , "_SingleProduct = prism' (\\{ a, b } -> SingleProduct a b) f"
                            , "  where"
                            , "    f (SingleProduct a b) = Just $ { a: a, b: b }"
                            , ""
                            , "--------------------------------------------------------------------------------"
                            ]
             in recTypeText `shouldBe` txt
        it "tests that sum types with multiple constructors don't generate record optics" $
            let recType = bridgeSumType (buildBridge defaultBridge) (mkSumType (Proxy :: Proxy TwoRecords))
                recTypeOptics = recordOptics recType
             in recTypeOptics `shouldBe` "" -- No record optics for multi-constructors
    describe "buildBridge without lens-code-gen and argonaut-codecs" $ do
        let settings = getSettings (noLenses <> useGen <> noArgonautCodecs)
        it "tests generation of for custom type Foo" $
            let proxy = Proxy :: Proxy Foo
                recType = bridgeSumType (buildBridge defaultBridge) (order proxy $ mkSumType proxy)
                recTypeText = sumTypeToText settings recType
                txt =
                    T.unlines
                        [ "data Foo ="
                        , "    Foo"
                        , "  | Bar Int"
                        , "  | FooBar Int String"
                        , ""
                        , "derive instance eqFoo :: Eq Foo"
                        , "derive instance ordFoo :: Ord Foo"
                        , "derive instance genericFoo :: Generic Foo"
                        ]
             in recTypeText `shouldBe` txt
        it "tests the generation of a whole (dummy) module" $
            let advanced' = bridgeSumType (buildBridge defaultBridge) (mkSumType (Proxy :: Proxy (Bar A B M1 C)))
                modules = sumTypeToModule advanced' Map.empty
                m = head . map (moduleToText settings) . Map.elems $ modules
                txt =
                    T.unlines
                        [ "-- File auto generated by purescript-bridge! --"
                        , "module TestData where"
                        , ""
                        , "import Data.Either (Either)"
                        , "import Data.Generic (class Generic)"
                        , "import Data.Maybe (Maybe, Maybe(..))"
                        , "import Data.Newtype (class Newtype)"
                        , ""
                        , "import Prelude"
                        , ""
                        , "data Bar a b m c ="
                        , "    Bar1 (Maybe a)"
                        , "  | Bar2 (Either a b)"
                        , "  | Bar3 a"
                        , "  | Bar4 {"
                        , "      myMonadicResult :: m b"
                        , "    }"
                        , ""
                        , "derive instance genericBar :: (Generic a, Generic b, Generic (m b)) => Generic (Bar a b m c)"
                        , ""
                        ]
             in m `shouldBe` txt
        it "tests generation of newtypes for record data type" $
            let recType' = bridgeSumType (buildBridge defaultBridge) (mkSumType (Proxy :: Proxy (SingleRecord A B)))
                recTypeText = sumTypeToText settings recType'
                txt =
                    T.unlines
                        [ "newtype SingleRecord a b ="
                        , "    SingleRecord {"
                        , "      _a :: a"
                        , "    , _b :: b"
                        , "    , c :: String"
                        , "    }"
                        , ""
                        , "derive instance genericSingleRecord :: (Generic a, Generic b) => Generic (SingleRecord a b)"
                        , "derive instance newtypeSingleRecord :: Newtype (SingleRecord a b) _"
                        ]
             in recTypeText `shouldBe` txt
        it "tests generation of newtypes for haskell newtype" $
            let recType' = bridgeSumType (buildBridge defaultBridge) (mkSumType (Proxy :: Proxy SomeNewtype))
                recTypeText = sumTypeToText settings recType'
                txt =
                    T.unlines
                        [ "newtype SomeNewtype ="
                        , "    SomeNewtype Int"
                        , ""
                        , "derive instance genericSomeNewtype :: Generic SomeNewtype"
                        , "derive instance newtypeSomeNewtype :: Newtype SomeNewtype _"
                        ]
             in recTypeText `shouldBe` txt
        it "tests generation of newtypes for haskell data type with one argument" $
            let recType' = bridgeSumType (buildBridge defaultBridge) (mkSumType (Proxy :: Proxy SingleValueConstr))
                recTypeText = sumTypeToText settings recType'
                txt =
                    T.unlines
                        [ "newtype SingleValueConstr ="
                        , "    SingleValueConstr Int"
                        , ""
                        , "derive instance genericSingleValueConstr :: Generic SingleValueConstr"
                        , "derive instance newtypeSingleValueConstr :: Newtype SingleValueConstr _"
                        ]
             in recTypeText `shouldBe` txt
        it "tests generation for haskell data type with one constructor, two arguments" $
            let recType' = bridgeSumType (buildBridge defaultBridge) (mkSumType (Proxy :: Proxy SingleProduct))
                recTypeText = sumTypeToText settings recType'
                txt =
                    T.unlines
                        [ "data SingleProduct ="
                        , "    SingleProduct String Int"
                        , ""
                        , "derive instance genericSingleProduct :: Generic SingleProduct"
                        ]
             in recTypeText `shouldBe` txt

    describe "buildBridge without lens-code-gen, generics-rep, and argonaut-codecs" $ do
        let settings = getSettings (noLenses <> useGenRep <> noArgonautCodecs)
        it "tests generation of for custom type Foo" $
            let proxy = Proxy :: Proxy Foo
                recType = bridgeSumType (buildBridge defaultBridge) (order proxy $ mkSumType proxy)
                recTypeText = sumTypeToText settings recType
                txt =
                    T.unlines
                        [ "data Foo ="
                        , "    Foo"
                        , "  | Bar Int"
                        , "  | FooBar Int String"
                        , ""
                        , "derive instance eqFoo :: Eq Foo"
                        , "derive instance ordFoo :: Ord Foo"
                        , "derive instance genericFoo :: Generic Foo _"
                        ]
             in recTypeText `shouldBe` txt
        it "tests the generation of a whole (dummy) module" $
            let advanced' = bridgeSumType (buildBridge defaultBridge) (mkSumType (Proxy :: Proxy (Bar A B M1 C)))
                modules = sumTypeToModule advanced' Map.empty
                m = head . map (moduleToText settings) . Map.elems $ modules
                txt =
                    T.unlines
                        [ "-- File auto generated by purescript-bridge! --"
                        , "module TestData where"
                        , ""
                        , "import Data.Either (Either)"
                        , "import Data.Generic.Rep (class Generic)"
                        , "import Data.Maybe (Maybe, Maybe(..))"
                        , "import Data.Newtype (class Newtype)"
                        , ""
                        , "import Prelude"
                        , ""
                        , "data Bar a b m c ="
                        , "    Bar1 (Maybe a)"
                        , "  | Bar2 (Either a b)"
                        , "  | Bar3 a"
                        , "  | Bar4 {"
                        , "      myMonadicResult :: m b"
                        , "    }"
                        , ""
                        , "derive instance genericBar :: (Generic a ra, Generic b rb, Generic (m b) rmb) => Generic (Bar a b m c) _"
                        , ""
                        ]
             in m `shouldBe` txt
        it "tests generation of newtypes for record data type" $
            let recType' = bridgeSumType (buildBridge defaultBridge) (mkSumType (Proxy :: Proxy (SingleRecord A B)))
                recTypeText = sumTypeToText settings recType'
                txt =
                    T.unlines
                        [ "newtype SingleRecord a b ="
                        , "    SingleRecord {"
                        , "      _a :: a"
                        , "    , _b :: b"
                        , "    , c :: String"
                        , "    }"
                        , ""
                        , "derive instance genericSingleRecord :: (Generic a ra, Generic b rb) => Generic (SingleRecord a b) _"
                        , "derive instance newtypeSingleRecord :: Newtype (SingleRecord a b) _"
                        ]
             in recTypeText `shouldBe` txt
        it "tests generation of newtypes for haskell newtype" $
            let recType' = bridgeSumType (buildBridge defaultBridge) (mkSumType (Proxy :: Proxy SomeNewtype))
                recTypeText = sumTypeToText settings recType'
                txt =
                    T.unlines
                        [ "newtype SomeNewtype ="
                        , "    SomeNewtype Int"
                        , ""
                        , "derive instance genericSomeNewtype :: Generic SomeNewtype _"
                        , "derive instance newtypeSomeNewtype :: Newtype SomeNewtype _"
                        ]
             in recTypeText `shouldBe` txt
        it "tests generation of newtypes for haskell data type with one argument" $
            let recType' = bridgeSumType (buildBridge defaultBridge) (mkSumType (Proxy :: Proxy SingleValueConstr))
                recTypeText = sumTypeToText settings recType'
                txt =
                    T.unlines
                        [ "newtype SingleValueConstr ="
                        , "    SingleValueConstr Int"
                        , ""
                        , "derive instance genericSingleValueConstr :: Generic SingleValueConstr _"
                        , "derive instance newtypeSingleValueConstr :: Newtype SingleValueConstr _"
                        ]
             in recTypeText `shouldBe` txt
        it "tests generation for haskell data type with one constructor, two arguments" $
            let recType' = bridgeSumType (buildBridge defaultBridge) (mkSumType (Proxy :: Proxy SingleProduct))
                recTypeText = sumTypeToText settings recType'
                txt =
                    T.unlines
                        [ "data SingleProduct ="
                        , "    SingleProduct String Int"
                        , ""
                        , "derive instance genericSingleProduct :: Generic SingleProduct _"
                        ]
             in recTypeText `shouldBe` txt

    describe "tests bridging Haskells Data.Word to PureScripts Data.Word from purescript-word" $ do
        describe "moduleToText" $
            it "should contain the right import and datatype" $ do
                let settings = getSettings (noLenses <> useGen <> noArgonautCodecs)
                    createModuleText :: SumType 'Haskell -> T.Text
                    createModuleText sumType =
                        let bridge = buildBridge defaultBridge
                            modules = sumTypeToModule (bridgeSumType bridge sumType) Map.empty
                         in head . map (moduleToText settings) . Map.elems $ modules
                    expectedText =
                        T.unlines
                            [ "-- File auto generated by purescript-bridge! --"
                            , "module TestData where"
                            , ""
                            , "import Data.Generic (class Generic)"
                            , "import Data.Maybe (Maybe(..))"
                            , "import Data.Newtype (class Newtype)"
                            , "import Data.Word (Word64)"
                            , ""
                            , "import Prelude"
                            , ""
                            , "newtype Simple Word64 ="
                            , "    Simple Word64"
                            , ""
                            , "derive instance genericSimple :: Generic Word64 => Generic (Simple Word64)"
                            , "derive instance newtypeSimple :: Newtype (Simple Word64) _"
                            , ""
                            ]
                createModuleText (mkSumType (Proxy @(Simple Word64))) `shouldBe` expectedText
        describe "buildBridge" $
            it "should create the correct type information for Word" $ do
                let bst = buildBridge defaultBridge (mkTypeInfo (Proxy @Word))
                    ti =
                        TypeInfo
                            { _typePackage = "purescript-word"
                            , _typeModule = "Data.Word"
                            , _typeName = "Word"
                            , _typeParameters = []
                            }
                 in bst `shouldBe` ti
