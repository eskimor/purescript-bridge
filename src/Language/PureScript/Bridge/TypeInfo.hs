{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}

module Language.PureScript.Bridge.TypeInfo
    ( TypeInfo (..)
    , PSType
    , HaskellType
    , mkTypeInfo
    , mkTypeInfo'
    , Language (..)
    , typePackage
    , typeModule
    , typeName
    , typeParameters
    , HasHaskType
    , haskType
    , flattenTypeInfo
    ) where

import           Control.Lens (Lens', makeLenses)
import           Data.Proxy (Proxy (Proxy))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable (TypeRep, Typeable, tyConModule, tyConName,
                                tyConPackage, typeRep, typeRepArgs,
                                typeRepTyCon)

data Language = Haskell | PureScript

-- | Basic info about a data type:
data TypeInfo (lang :: Language) = TypeInfo
  { _typePackage    :: !Text
    -- ^ Hackage package
  , _typeModule     :: !Text
    -- ^ Full Module path
  , _typeName       :: !Text
  , _typeParameters :: ![TypeInfo lang]
  }
  deriving (Eq, Ord, Show)

makeLenses ''TypeInfo

-- | For convenience:
type PSType = TypeInfo 'PureScript

-- | For convenience:
type HaskellType = TypeInfo 'Haskell

-- | Types that have a lens for accessing a 'TypeInfo Haskell'.
class HasHaskType t where
    haskType :: Lens' t HaskellType

-- | Simple 'id' instance: Get the 'TypeInfo' itself.
instance HasHaskType HaskellType where
    haskType inj = inj

mkTypeInfo :: forall t. (Typeable t) => HaskellType
mkTypeInfo = mkTypeInfo' . typeRep $ Proxy @t

mkTypeInfo' :: TypeRep -> HaskellType
mkTypeInfo' rep =
    let con = typeRepTyCon rep
    in TypeInfo
        { _typePackage = T.pack $ tyConPackage con
        , _typeModule = T.pack $ tyConModule con
        , _typeName = T.pack $ tyConName con
        , _typeParameters = map mkTypeInfo' (typeRepArgs rep)
        }

-- | Put the TypeInfo in a list together with all its '_typeParameters' (recursively)
flattenTypeInfo :: TypeInfo lang -> [TypeInfo lang]
flattenTypeInfo t = t : concatMap flattenTypeInfo (_typeParameters t)
