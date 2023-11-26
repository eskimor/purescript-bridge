{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE FlexibleInstances #-}

module Types where

import           Control.Lens.TH (makeLenses)
import           Data.Aeson (FromJSON, ToJSON(toEncoding), SumEncoding(..), defaultOptions, tagSingleConstructors, genericToEncoding, unwrapUnaryRecords, sumEncoding, defaultTaggedObject)
import qualified Data.Map.Lazy as Map
import           Data.Proxy
import           Data.Text
import           Data.Typeable
import           GHC.Generics
import           Language.PureScript.Bridge (BridgePart, Language(Haskell), mkSumType, argonautAesonGeneric, lenses, genericShow, defaultBridge)
import           Language.PureScript.Bridge.PSTypes
import           Language.PureScript.Bridge.SumType (SumType)
import           Language.PureScript.Bridge.TypeParameters (A)
import           Test.QuickCheck (Arbitrary (..), chooseEnum, oneof, resize,
                                  sized)

data Baz = Baz
  { _bazMessage :: Text
  }
  deriving (FromJSON, Generic, Show)

instance ToJSON Baz where
  toEncoding = genericToEncoding
    (
      defaultOptions
      { tagSingleConstructors = True
      , unwrapUnaryRecords = True
      }
    )


makeLenses ''Baz

data TestSum
  = Nullary
  | Bool Bool
  | Int Int
  | Number Double
  deriving (Eq, Generic, Ord, Show, FromJSON, ToJSON)

instance Arbitrary Text where
    arbitrary = pure $ pack "foooo"

instance Arbitrary TestSum where
    arbitrary =
        oneof
            [ pure Nullary
            , Bool <$> arbitrary
            , Int <$> arbitrary
            , Number <$> arbitrary
            ]

data TestData
  = TMaybe (Maybe TestSum)
  | TEither Text -- (Either Int Text) -- (Either (Maybe Int) (Maybe Bool))
  deriving (Eq, Generic, Ord, Show, FromJSON, ToJSON)

instance Arbitrary TestData where
    arbitrary =
        oneof
            [ -- Maybe <$> arbitrary
            -- ,
              TEither <$> arbitrary
            ]


data Foo = Foo
  { _fooMessage :: Text
  , _fooE       :: Either Text Int
  , _fooNumber  :: Int
  , _fooList    :: [Int]
  , _fooMap     :: Map.Map Text Int
  , _fooBaz     :: Baz
  , _fooTestSum  :: TestSum
  , _fooTestData :: TestData
  }
  deriving (FromJSON, Generic, Show, ToJSON)



instance {-# OVERLAPPING #-} ToJSON (Either Text Int) where
  toEncoding = genericToEncoding
    (
      defaultOptions
      { tagSingleConstructors = True
      , unwrapUnaryRecords = True
      -- , sumEncoding = TaggedObject "foo" "bar" -- defaultTaggedObject { contentsFieldName = "value" }
      }
    )

instance {-# OVERLAPPING #-} FromJSON (Either Text Int)

makeLenses ''Foo

-- TODO newtype
data Bar a = Bar a
  deriving (FromJSON, Generic, Show, Typeable, ToJSON)

makeLenses ''Bar

myBridge :: BridgePart
myBridge = defaultBridge

additionalInstances = lenses
  . genericShow
  . argonautAesonGeneric

  -- . jsonHelper
  -- To use json-helpers with the example, a modification is necessary
  -- in Main.purs

myTypes :: [SumType 'Haskell]
myTypes =
  [ additionalInstances $ mkSumType @Baz
  , additionalInstances $ mkSumType @Foo
  , additionalInstances $ mkSumType @(Bar A)
  , additionalInstances $ mkSumType @TestSum
  , additionalInstances $ mkSumType @TestData
  ]
