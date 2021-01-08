{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE DuplicateRecordFields #-}

module TestData where

import           Data.Proxy
import           Data.Text                          (Text)
import           Data.Typeable
import           GHC.Generics                       (Generic)
import           Language.PureScript.Bridge
import           Language.PureScript.Bridge.PSTypes



-- Check that examples compile:
textBridge :: BridgePart
textBridge = do
   typeName ^== "Text"
   typeModule ^== "Data.Text.Internal" <|> typeModule ^== "Data.Text.Internal.Lazy"
   return psString

stringBridge :: BridgePart
stringBridge = do
   haskType ^== mkTypeInfo (Proxy :: Proxy String)
   return psString

data Foo = Foo
         | Bar Int
         | FooBar Int Text
         deriving (Eq, Ord, Generic, Typeable, Show)

data Test = TestIntInt Int Int
          | TestBool {bool :: Bool}
          | TestVoid
          deriving (Generic, Typeable, Show)

data Bar a b m c = Bar1 (Maybe a) | Bar2 (Either a b) | Bar3 a
                 | Bar4 { myMonadicResult :: m b }
                 deriving (Generic, Typeable, Show)

data SingleRecord a b = SingleRecord {
    _a :: a
  , _b :: b
  , c  :: String
  } deriving(Generic, Typeable, Show)

data SingleRecordWithDuplicateFields a b = SingleRecordWithDuplicateFields {
    _a :: a
  , _b :: b
  , c  :: String
  } deriving(Generic, Typeable, Show)

data TwoRecords
  = FirstRecord {
    _fra :: String
  , _frb :: Int
  }
  | SecondRecord {
    _src :: Int
  , _srd :: [Int]
  } deriving(Generic, Typeable, Show)

newtype SomeNewtype = SomeNewtype Int
  deriving (Generic, Typeable, Show)

data SingleValueConstr = SingleValueConstr Int
  deriving (Generic, Typeable, Show)

data SingleProduct = SingleProduct Text Int
  deriving (Generic, Typeable, Show)

a :: HaskellType
a = mkTypeInfo (Proxy :: Proxy (Either String Int))

applyBridge :: FullBridge
applyBridge = buildBridge defaultBridge

psA :: PSType
psA = applyBridge a

b :: SumType 'Haskell
b = mkSumType (Proxy :: Proxy (Either String Int))

t :: TypeInfo 'PureScript
cs :: [DataConstructor 'PureScript]
psB :: SumType 'PureScript
psB@(SumType t cs _) = bridgeSumType (buildBridge defaultBridge) b
