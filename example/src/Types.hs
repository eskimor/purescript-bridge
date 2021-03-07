{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Types where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Proxy
import Data.Text
import GHC.Generics
import Language.PureScript.Bridge
import Language.PureScript.Bridge.PSTypes
import qualified Data.Map.Lazy as Map

data Foo = Foo
  { _fooMessage :: Text
  , _fooNumber  :: Int
  , _fooList    :: [Int]
  , _fooMap     :: Map.Map Text Int
  } deriving (Generic, ToJSON, FromJSON)

makeLenses ''Foo

fooProxy :: Proxy Foo
fooProxy = Proxy

myBridge :: BridgePart
myBridge = defaultBridge

myTypes :: [SumType 'Haskell]
myTypes =
  [ mkSumType (Proxy :: Proxy Foo)
  ]
