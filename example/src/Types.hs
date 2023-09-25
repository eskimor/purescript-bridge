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
import qualified Language.PureScript.Bridge.SumType as SumType
import           Language.PureScript.Bridge.TypeParameters (A)

data Baz = Baz
  { _bazMessage :: Text
  }
  deriving (FromJSON, Generic, ToJSON)

makeLenses ''Baz

bazProxy :: Proxy Baz
bazProxy = Proxy

data Foo = Foo
  { _fooMessage :: Text
  , _fooNumber  :: Int
  , _fooList    :: [Int]
  , _fooMap     :: Map.Map Text Int
  , _fooBaz     :: Baz
  }
  deriving (FromJSON, Generic, ToJSON)

makeLenses ''Foo

fooProxy :: Proxy Foo
fooProxy = Proxy

-- TODO newtype
data Bar a = Bar a
  deriving (FromJSON, Generic, Show, ToJSON, Typeable)

makeLenses ''Bar

barProxy :: Proxy Bar
barProxy = Proxy

myBridge :: BridgePart
myBridge = defaultBridge

additionalInstances = SumType.lenses . SumType.genericShow . SumType.argonautJson

myTypes :: [SumType 'Haskell]
myTypes =
  [ additionalInstances $ mkSumType @Baz
  , additionalInstances $ mkSumType @Foo
  , additionalInstances $ mkSumType @(Bar A)
  ]
