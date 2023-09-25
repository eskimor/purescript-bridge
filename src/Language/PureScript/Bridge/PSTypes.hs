{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- | PureScript types to be used for bridges, e.g. in "Language.PureScript.Bridge.Primitives".
module Language.PureScript.Bridge.PSTypes where

import           Control.Lens (view)
import           Control.Monad.Reader.Class
import           Language.PureScript.Bridge.Builder
import           Language.PureScript.Bridge.TypeInfo

-- | Uses  type parameters from 'haskType' (bridged).
psArray :: (MonadReader BridgeData m) => m PSType
psArray = TypeInfo "" "Prim" "Array" <$> psTypeParameters

psBool :: PSType
psBool =
    TypeInfo
        { _typePackage = ""
        , _typeModule = "Prim"
        , _typeName = "Boolean"
        , _typeParameters = []
        }

-- | Uses  type parameters from 'haskType' (bridged).
psEither :: (MonadReader BridgeData m) => m PSType
psEither =
    TypeInfo "purescript-either" "Data.Either" "Either" <$> psTypeParameters

psObject :: MonadReader BridgeData m => m PSType
psObject = do
    valueTypes <- tail <$> psTypeParameters
    return $ TypeInfo "purescript-foreign-object" "Foreign.Object" "Object" valueTypes

psInt :: PSType
psInt =
    TypeInfo
        { _typePackage = ""
        , _typeModule = "Prim"
        , _typeName = "Int"
        , _typeParameters = []
        }

psNumber :: PSType
psNumber =
    TypeInfo
        { _typePackage = ""
        , _typeModule = "Prim"
        , _typeName = "Number"
        , _typeParameters = []
        }

-- | Uses  type parameters from 'haskType' (bridged).
psMaybe :: (MonadReader BridgeData m) => m PSType
psMaybe = TypeInfo "purescript-maybe" "Data.Maybe" "Maybe" <$> psTypeParameters

psString :: PSType
psString =
    TypeInfo
        { _typePackage = ""
        , _typeModule = "Prim"
        , _typeName = "String"
        , _typeParameters = []
        }

-- | Uses  type parameters from 'haskType' (bridged).
psTuple :: (MonadReader BridgeData m) => m PSType
psTuple = do
    params <- view (haskType . typeParameters)
    bridge <- view fullBridge
    let computeTuple [] = psUnit
        computeTuple [a] = bridge a
        computeTuple [a, b] = TypeInfo "purescript-tuples" "Data.Tuple" "Tuple" [bridge a, bridge b]
        computeTuple (h : t) = TypeInfo "purescript-tuples" "Data.Tuple" "Tuple" [bridge h, computeTuple t]
    pure $ computeTuple params

psUnit :: PSType
psUnit =
    TypeInfo
        { _typePackage = "purescript-prelude"
        , _typeModule = "Prelude"
        , _typeName = "Unit"
        , _typeParameters = []
        }

psWord :: PSType
psWord =
    TypeInfo
        { _typePackage = "purescript-word"
        , _typeModule = "Data.Word"
        , _typeName = "Word"
        , _typeParameters = []
        }

psWord8 :: PSType
psWord8 =
    TypeInfo
        { _typePackage = "purescript-word"
        , _typeModule = "Data.Word"
        , _typeName = "Word8"
        , _typeParameters = []
        }

psWord16 :: PSType
psWord16 =
    TypeInfo
        { _typePackage = "purescript-word"
        , _typeModule = "Data.Word"
        , _typeName = "Word16"
        , _typeParameters = []
        }

psWord32 :: PSType
psWord32 =
    TypeInfo
        { _typePackage = "purescript-word"
        , _typeModule = "Data.Word"
        , _typeName = "Word32"
        , _typeParameters = []
        }

psWord64 :: PSType
psWord64 =
    TypeInfo
        { _typePackage = "purescript-word"
        , _typeModule = "Data.Word"
        , _typeName = "Word64"
        , _typeParameters = []
        }

psMap :: (MonadReader BridgeData m) => m PSType
psMap =
    TypeInfo "purescript-ordered-collections" "Data.Map" "Map" <$> psTypeParameters

psSet :: (MonadReader BridgeData m) => m PSType
psSet =
    TypeInfo "purescript-ordered-collections" "Data.Set" "Set" <$> psTypeParameters
