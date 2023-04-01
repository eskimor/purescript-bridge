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
import           GHC.Generics
import           Language.PureScript.Bridge
import           Language.PureScript.Bridge.PSTypes

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

myBridge :: BridgePart
myBridge = defaultBridge

myTypes :: [SumType 'Haskell]
myTypes =
  [ mkSumType @Baz
  , mkSumType @Foo
  ]
