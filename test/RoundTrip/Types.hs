{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications           #-}

module RoundTrip.Types where

import           Control.Applicative ((<|>))
import           Data.Aeson (FromJSON, ToJSON)
import           Data.Map (Map)
import           Data.Proxy (Proxy (..))
import           Data.Set (Set)
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Language.PureScript.Bridge (BridgePart, Language (..), SumType,
                                             buildBridge, defaultBridge,
                                             defaultSwitch, mkSumType,
                                             writePSTypes, writePSTypesWith)
import           Language.PureScript.Bridge.TypeParameters (A)
import           System.Directory (removeDirectoryRecursive, removeFile,
                                   withCurrentDirectory)
import           System.Exit (ExitCode (ExitSuccess))
import           System.Process (readProcessWithExitCode)
import           Test.Hspec (Spec, aroundAll_, describe, it)
import           Test.Hspec.Expectations.Pretty (shouldBe)
import           Test.HUnit (assertEqual)
import           Test.QuickCheck (Arbitrary (..), chooseEnum, oneof, resize,
                                  sized)

data TestData
  = Maybe (Maybe TestSum)
  | Either (Either (Maybe Int) (Maybe Bool))
  deriving (Eq, Generic, Ord, Show)

instance FromJSON TestData

instance ToJSON TestData

instance Arbitrary TestData where
    arbitrary =
        oneof
            [ Maybe <$> arbitrary
            , Either <$> arbitrary
            ]

data TestSum
  = Nullary
  | Bool Bool
  | Int Int
  | Number Double
  | String String
  | Array [Int]
  | InlineRecord
  { why            :: String
  , wouldYouDoThis :: Int
  }
  | MultiInlineRecords TestMultiInlineRecords
  | Record (TestRecord Int)
  | NestedRecord (TestRecord (TestRecord Int))
  | NT TestNewtype
  | NTRecord TestNewtypeRecord
  | TwoFields TestTwoFields
  | Set (Set Int)
  | Map (Map String Int)
  | Unit ()
  | MyUnit MyUnit
  | Pair (Int, Double)
  | Triple (Int, (), Bool)
  | Quad (Int, Double, Bool, Double)
  | QuadSimple Int Double Bool Double
  | Recursive TestRecursiveA
  | Enum TestEnum
  deriving (Eq, Generic, Ord, Show)

instance FromJSON TestSum

instance ToJSON TestSum

instance Arbitrary TestSum where
    arbitrary =
        oneof
            [ pure Nullary
            , Bool <$> arbitrary
            , Int <$> arbitrary
            , Number <$> arbitrary
            , String <$> arbitrary
            , Array <$> arbitrary
            , InlineRecord <$> arbitrary <*> arbitrary
            , MultiInlineRecords <$> arbitrary
            , Record <$> arbitrary
            , NestedRecord <$> arbitrary
            , NT <$> arbitrary
            , NTRecord <$> arbitrary
            , Map <$> arbitrary
            , Set <$> arbitrary
            , TwoFields <$> arbitrary
            , pure $ Unit ()
            , Pair <$> arbitrary
            , Triple <$> arbitrary
            , Quad <$> arbitrary
            , QuadSimple <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
            , Enum <$> arbitrary
            ]

data TestRecursiveA
  = Nil
  | Recurse TestRecursiveB
  deriving (Eq, Generic, Ord, Show)

instance FromJSON TestRecursiveA

instance ToJSON TestRecursiveA

instance Arbitrary TestRecursiveA where
    arbitrary = sized go
      where
        go size
            | size > 0 = oneof [pure Nil, resize (size - 1) $ Recurse <$> arbitrary]
            | otherwise = pure Nil

newtype TestRecursiveB
  = RecurseB TestRecursiveB
  deriving (Arbitrary, Eq, Generic, Ord, Show)

instance FromJSON TestRecursiveB

instance ToJSON TestRecursiveB

data TestMultiInlineRecords
  = Foo
  { _foo1 :: Maybe Int
  , _foo2 :: ()
  }
  | Bar
  { _bar1 :: String
  , _bar2 :: Bool
  }
  deriving (Eq, Generic, Ord, Show)

instance FromJSON TestMultiInlineRecords

instance ToJSON TestMultiInlineRecords

instance Arbitrary TestMultiInlineRecords where
    arbitrary =
        oneof
            [ Foo <$> arbitrary <*> arbitrary
            , Bar <$> arbitrary <*> arbitrary
            ]

data TestRecord a = TestRecord
  { _field1 :: Maybe Int
  , _field2 :: a
  }
  deriving (Eq, Generic, Ord, Show)

instance (FromJSON a) => FromJSON (TestRecord a)

instance (ToJSON a) => ToJSON (TestRecord a)

instance (Arbitrary a) => Arbitrary (TestRecord a) where
    arbitrary = TestRecord <$> arbitrary <*> arbitrary

data TestTwoFields = TestTwoFields Bool Int
  deriving (Eq, Generic, Ord, Show)

instance FromJSON TestTwoFields

instance ToJSON TestTwoFields

instance Arbitrary TestTwoFields where
    arbitrary = TestTwoFields <$> arbitrary <*> arbitrary

newtype TestNewtype
  = TestNewtype (TestRecord Bool)
  deriving (Eq, Generic, Ord, Show)

instance FromJSON TestNewtype

instance ToJSON TestNewtype

instance Arbitrary TestNewtype where
    arbitrary = TestNewtype <$> arbitrary

newtype TestNewtypeRecord
  = TestNewtypeRecord { unTestNewtypeRecord :: TestNewtype }
  deriving (Eq, Generic, Ord, Show)

instance FromJSON TestNewtypeRecord

instance ToJSON TestNewtypeRecord

instance Arbitrary TestNewtypeRecord where
    arbitrary = TestNewtypeRecord <$> arbitrary

data TestEnum = Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Bounded, Enum, Eq, Generic, Ord, Show)

instance FromJSON TestEnum

instance ToJSON TestEnum

instance Arbitrary TestEnum where
    arbitrary = chooseEnum (minBound, maxBound)

data MyUnit = U
  deriving (Bounded, Enum, Eq, Generic, Ord, Show)

instance FromJSON MyUnit

instance ToJSON MyUnit

instance Arbitrary MyUnit where
    arbitrary = pure U
