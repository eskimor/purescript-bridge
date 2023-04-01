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
import           Data.Text
import           GHC.Generics (Generic)
import           Language.PureScript.Bridge
import           Language.PureScript.Bridge.PSTypes
import           Language.PureScript.Bridge.TypeParameters
import           Data.Typeable

data Baz = Baz
  { _bazMessage :: Text
  }
  deriving (FromJSON, Generic, ToJSON)

makeLenses ''Baz

data Foo = Foo
  { _fooMessage :: Text
  , _fooNumber  :: Int
  , _fooList    :: [Int]
  , _fooMap     :: Map.Map Text Int
  , _fooBaz     :: Baz
  }
  deriving (FromJSON, Generic, ToJSON)

makeLenses ''Foo

-- TODO newtype
data BarSimple a = BarSimple a
  deriving (Generic, Show, Typeable, FromJSON, ToJSON)

makeLenses ''BarSimple

data Bar a b m c
  = Bar1 (Maybe a)
  | Bar2 (Either a b)
  | Bar3 a
  | Bar4 { myMonadicResult :: m b }

myBridge :: BridgePart
myBridge = defaultBridge

myInstances =
    argonautAesonGeneric
  . genericShow
  . equal
  . order
  . prisms
  . lenses

myTypes :: [SumType 'Haskell]
myTypes =
  [ myInstances $ mkSumType @Baz
  , myInstances $ mkSumType @Foo
  , myInstances $ mkSumType @(BarSimple A)
  -- , myInstances $ mkSumType @(Bar A B M1 C)
  ]
