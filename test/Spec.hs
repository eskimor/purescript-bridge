{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Language.PureScript.Bridge
import Language.PureScript.Bridge.CodeGenSwitches
import Language.PureScript.Bridge.TypeParameters
import RoundTrip.Spec (roundtripSpec)
import Test.Hspec
  ( Spec,
    describe,
    hspec,
    it,
  )
import Test.Hspec.Expectations.Pretty
import TestData
import Text.PrettyPrint.Leijen.Text
  ( Doc,
    cat,
    linebreak,
    punctuate,
    vsep,
  )

main :: IO ()
main = hspec $ allTests *> roundtripSpec

custom :: SumType 'Haskell -> SumType 'Haskell
custom (SumType t cs is) = SumType t cs $ customInstance : is
  where
    customInstance =
      Custom $
        CustomInstance [] (TypeInfo "" "Data.MyClass" "MyClass" [TypeInfo "" "" "Foo" []]) $
          Explicit
            [ InstanceMember "member1" ["foo", "bar"] "undefined" [] mempty,
              InstanceMember "member2" [] "do\npure unit" [] mempty
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
          [ TypeInfo "" "" "Eq" [TypeInfo "" "" "Foo" []],
            TypeInfo "" "" "Show" [TypeInfo "" "" "Foo" []]
          ]
          (TypeInfo "" "Data.MyDClass" "MyDClass" [TypeInfo "" "" "Foo" []])
          Derive

allTests :: Spec
allTests = do
  describe "buildBridge without lens-code-gen" $ do
    let settings = getSettings noLenses
    it "tests generation of custom typeclasses" $
      let sumType =
            bridgeSumType
              (buildBridge defaultBridge)
              (customNewtypeDerived . customDerived . custom $ mkSumType @Foo)
          doc = vsep $ sumTypeToDocs settings sumType
          txt =
            T.unlines
              [ "data Foo",
                "  = Foo",
                "  | Bar Int",
                "  | FooBar Int String",
                "",
                "derive newtype instance (Eq Foo) => MyNTClass Foo",
                "",
                "derive instance (Eq Foo, Show Foo) => MyDClass Foo",
                "",
                "instance MyClass Foo where",
                "  member1 foo bar = undefined",
                "  member2 = do",
                "    pure unit",
                "",
                "derive instance Generic Foo _"
              ]
       in doc `shouldRender` txt
    it "tests generation of typeclasses for custom type Foo" $
      let sumType =
            bridgeSumType
              (buildBridge defaultBridge)
              (genericShow . order $ mkSumType @Foo)
          doc = vsep $ sumTypeToDocs settings sumType
          txt =
            T.unlines
              [ "data Foo",
                "  = Foo",
                "  | Bar Int",
                "  | FooBar Int String",
                "",
                "instance Show Foo where",
                "  show a = genericShow a",
                "",
                "derive instance Eq Foo",
                "",
                "derive instance Ord Foo",
                "",
                "derive instance Generic Foo _"
              ]
       in doc `shouldRender` txt
    it "tests generation of typeclasses for custom type Func" $
      let sumType =
            bridgeSumType
              (buildBridge defaultBridge)
              (equal1 . functor . genericShow $ mkSumType @(Func A))
          doc = vsep $ sumTypeToDocs settings sumType
          txt =
            T.unlines
              [ "data Func a = Func Int a",
                "",
                "derive instance Eq1 Func",
                "",
                "derive instance Functor Func",
                "",
                "instance (Show a) => Show (Func a) where",
                "  show a = genericShow a",
                "",
                "derive instance Generic (Func a) _"
              ]
       in doc `shouldRender` txt
    it "tests the generation of a whole (dummy) module" $
      let advanced' :: SumType 'PureScript
          advanced' =
            bridgeSumType
              (buildBridge defaultBridge)
              (mkSumType @(Bar A B M1 C))
          modules :: Modules
          modules = sumTypeToModule Nothing advanced'
          m = head . map (moduleToText settings) . Map.elems $ modules
          txt =
            T.unlines
              [ "-- File auto generated by purescript-bridge! --",
                "module TestData where",
                "",
                "import Prelude",
                "",
                "import Data.Either (Either)",
                "import Data.Generic.Rep (class Generic)",
                "import Data.Maybe (Maybe(..))",
                "",
                "data Bar a b m c",
                "  = Bar1 (Maybe a)",
                "  | Bar2 (Either a b)",
                "  | Bar3 a",
                "  | Bar4 { myMonadicResult :: m b }",
                "",
                "derive instance Generic (Bar a b m c) _"
              ]
       in m `shouldBe` txt
    it "tests generation of newtypes for record data type" $
      let recType' =
            bridgeSumType
              (buildBridge defaultBridge)
              (mkSumType @(SingleRecord A B))
          doc = vsep $ sumTypeToDocs settings recType'
          txt =
            T.unlines
              [ "newtype SingleRecord a b = SingleRecord",
                "  { _a :: a",
                "  , _b :: b",
                "  , c :: String",
                "  }",
                "",
                "derive instance Generic (SingleRecord a b) _",
                "",
                "derive instance Newtype (SingleRecord a b) _"
              ]
       in doc `shouldRender` txt
    it "tests generation of newtypes for haskell newtype" $
      let recType' =
            bridgeSumType
              (buildBridge defaultBridge)
              (mkSumType @SomeNewtype)
          doc = vsep $ sumTypeToDocs settings recType'
          txt =
            T.unlines
              [ "newtype SomeNewtype = SomeNewtype Int",
                "",
                "derive instance Generic SomeNewtype _",
                "",
                "derive instance Newtype SomeNewtype _"
              ]
       in doc `shouldRender` txt
    it "tests generation of newtypes for haskell data type with one argument" $
      let recType' =
            bridgeSumType
              (buildBridge defaultBridge)
              (mkSumType @SingleValueConstr)
          doc = vsep $ sumTypeToDocs settings recType'
          txt =
            T.unlines
              [ "newtype SingleValueConstr = SingleValueConstr Int",
                "",
                "derive instance Generic SingleValueConstr _",
                "",
                "derive instance Newtype SingleValueConstr _"
              ]
       in doc `shouldRender` txt
    it
      "tests generation for haskell data type with one constructor, two arguments"
      $ let recType' =
              bridgeSumType
                (buildBridge defaultBridge)
                (mkSumType @SingleProduct)
            doc = vsep $ sumTypeToDocs settings recType'
            txt =
              T.unlines
                [ "data SingleProduct = SingleProduct String Int",
                  "",
                  "derive instance Generic SingleProduct _"
                ]
         in doc `shouldRender` txt
    it "tests generation Eq instances for polymorphic types" $
      let recType' =
            bridgeSumType
              (buildBridge defaultBridge)
              (equal $ mkSumType @(SingleRecord A B))
          doc = vsep $ sumTypeToDocs settings recType'
          txt =
            T.unlines
              [ "newtype SingleRecord a b = SingleRecord",
                "  { _a :: a",
                "  , _b :: b",
                "  , c :: String",
                "  }",
                "",
                "derive instance (Eq a, Eq b) => Eq (SingleRecord a b)",
                "",
                "derive instance Generic (SingleRecord a b) _",
                "",
                "derive instance Newtype (SingleRecord a b) _"
              ]
       in doc `shouldRender` txt
    it "tests generation of Ord instances for polymorphic types" $
      let recType' =
            bridgeSumType
              (buildBridge defaultBridge)
              (order $ mkSumType @(SingleRecord A B))
          doc = vsep $ sumTypeToDocs settings recType'
          txt =
            T.unlines
              [ "newtype SingleRecord a b = SingleRecord",
                "  { _a :: a",
                "  , _b :: b",
                "  , c :: String",
                "  }",
                "",
                "derive instance (Eq a, Eq b) => Eq (SingleRecord a b)",
                "",
                "derive instance (Ord a, Ord b) => Ord (SingleRecord a b)",
                "",
                "derive instance Generic (SingleRecord a b) _",
                "",
                "derive instance Newtype (SingleRecord a b) _"
              ]
       in doc `shouldRender` txt

shouldRender :: Doc -> Text -> Expectation
shouldRender actual expected = renderText actual `shouldBe` T.stripEnd expected
