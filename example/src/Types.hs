{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Types where

import           Control.Lens.TH (makeLenses)
import           Data.Aeson
import qualified Data.Map.Lazy as Map
import           Data.Proxy
import           Data.Text
import           Data.Typeable
import           GHC.Generics
import           Language.PureScript.Bridge
import           Language.PureScript.Bridge.PSTypes
import           Language.PureScript.Bridge.SumType
import           Language.PureScript.Bridge.TypeParameters (A)

data Baz = Baz
  { _bazMessage :: Text
  }
  deriving (FromJSON, Generic, ToJSON)

makeLenses ''Baz

data TestSum
  = Nullary
  | Bool Bool
  | Int Int
  | Number Double
  deriving (Eq, Generic, Ord, Show, FromJSON, ToJSON)

data TestData
  = Maybe (Maybe TestSum)
  | Either (Either (Maybe Int) (Maybe Bool))
  deriving (Eq, Generic, Ord, Show, FromJSON, ToJSON)


data Foo = Foo
  { _fooMessage :: Text
  , _fooNumber  :: Int
  , _fooList    :: [Int]
  , _fooMap     :: Map.Map Text Int
  , _fooBaz     :: Baz
  , _fooTestData :: TestData
  , _fooTestSum  :: TestSum
  }
  deriving (FromJSON, Generic, ToJSON)

makeLenses ''Foo

-- TODO newtype
data Bar a = Bar a
  deriving (FromJSON, Generic, Show, ToJSON, Typeable)

makeLenses ''Bar

myBridge :: BridgePart
myBridge = defaultBridge

additionalInstances = lenses
  . genericShow
  -- . jsonHelper
  . argonautAesonGeneric

myTypes :: [SumType 'Haskell]
myTypes =
  [ additionalInstances $ mkSumType @Baz
  , additionalInstances $ mkSumType @Foo
  , additionalInstances $ mkSumType @(Bar A)
  , additionalInstances $ mkSumType @TestSum
  , additionalInstances $ mkSumType @TestData
  ]
