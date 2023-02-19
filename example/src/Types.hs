{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Types where

import           Control.Lens.TH (makeLenses)
import           Data.Aeson
import qualified Data.Map.Lazy as Map
import           Data.Proxy
import           Data.Text
import           GHC.Generics
import           Language.PureScript.Bridge
import           Language.PureScript.Bridge.PSTypes

data Baz = Baz
  { _bazMessage :: Text
  } deriving (Generic, ToJSON, FromJSON)

makeLenses ''Baz

bazProxy :: Proxy Baz
bazProxy = Proxy

data Foo = Foo
  { _fooMessage :: Text
  , _fooNumber  :: Int
  , _fooList    :: [Int]
  , _fooMap     :: Map.Map Text Int
  , _fooBaz     :: Baz
  } deriving (Generic, ToJSON, FromJSON)

makeLenses ''Foo

fooProxy :: Proxy Foo
fooProxy = Proxy

myBridge :: BridgePart
myBridge = defaultBridge

myTypes :: [SumType 'Haskell]
myTypes =
  [ mkSumType (Proxy :: Proxy Baz)
  , mkSumType (Proxy :: Proxy Foo)
  ]
