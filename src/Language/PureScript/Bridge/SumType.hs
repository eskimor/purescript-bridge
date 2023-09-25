{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.PureScript.Bridge.SumType
    ( SumType (..)
    , mkSumType
    , equal
    , equal1
    , order
    , argonautJson
    , genericShow
    , functor
    , DataConstructor (..)
    , GDataConstructor
    , RecordEntry (..)
    , Instance (..)
    , ImportLine (..)
    , ImportLines
    , InstanceMember (..)
    , memberName
    , memberBindings
    , memberBody
    , memberDependencies
    , memberImportLines
    , DataConstructorArgs (..)
    , CustomInstance (..)
    , customConstraints
    , customHead
    , customImplementation
    , InstanceImplementation (..)
    , PSInstance
    , importsFromList
    , instanceToImportLines
    , nootype
    , lenses
    , prisms
    , getUsedTypes
    , constructorToTypes
    , sigConstructor
    , sigValues
    , sumTypeInfo
    , sumTypeConstructors
    , recLabel
    , recValue
    ) where

import           Control.Lens hiding (from, to)
import           Data.List (nub)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (maybeToList)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable
import           Generics.Deriving
import           Language.PureScript.Bridge.TypeInfo

data ImportLine = ImportLine
  { importModule :: !Text
  , importAlias  :: !(Maybe Text)
  , importTypes  :: !(Set Text)
  }
  deriving (Eq, Ord, Show)

type ImportLines = Map Text ImportLine

-- | Generic representation of your Haskell types.
data SumType (lang :: Language) = SumType (TypeInfo lang) [DataConstructor lang] [Instance lang]
  deriving (Eq, Show)

-- | TypeInfo lens for 'SumType'.
sumTypeInfo
    :: (Functor f)
    => (TypeInfo lang -> f (TypeInfo lang))
    -> SumType lang
    -> f (SumType lang)
sumTypeInfo inj (SumType info constrs is) =
    (\ti -> SumType ti constrs is) <$> inj info

-- | DataConstructor lens for 'SumType'.
sumTypeConstructors
    :: (Functor f)
    => ([DataConstructor lang] -> f [DataConstructor lang])
    -> SumType lang
    -> f (SumType lang)
sumTypeConstructors inj (SumType info constrs is) =
    (\cs -> SumType info cs is) <$> inj constrs

{- | Create a representation of your sum (and product) types,
  for doing type translations and writing it out to your PureScript modules.
-}
mkSumType
    :: forall t
     . (Generic t, Typeable t, GDataConstructor (Rep t))
    => SumType 'Haskell
mkSumType =
    SumType
        (mkTypeInfo @t)
        constructors
        (Generic : maybeToList (nootype constructors))
  where
    constructors = gToConstructors (from (undefined :: t))

-- | Purescript typeclass instances that can be generated for your Haskell types.
data Instance (lang :: Language)
  = Generic
  | GenericShow
  -- | Generate
  -- `Data.Argonaut.Decode.Class EncodeJson` instances
  -- using argonaut-codecs:
  -- <https://pursuit.purescript.org/packages/purescript-argonaut-codecs>
  | EncodeJson
  -- | Generate
  -- `Data.Argonaut.Decode.Class DecodeJson` instances
  -- using argonaut-codecs:
  -- <https://pursuit.purescript.org/packages/purescript-argonaut-codecs>
  | DecodeJson
  -- | Generate `Foreign.Generic` and `Foreign.Object`
  -- using purescript-foreign-generic:
  -- <https://pursuit.purescript.org/packages/purescript-foreign-generic>
  -- and purescript-foreign-object:
  -- <https://pursuit.purescript.org/packages/purescript-foreign-object>
  | ForeignObject
  { unwrapSingleConstructors :: Bool
  , unwrapSingleArguments    :: Bool
  }
  | Newtype
  | Functor
  | Eq
  | Eq1
  | Ord
  | Enum
  | Bounded
  -- | Generate lenses using profunctor-lenses:
  -- <https://pursuit.purescript.org/packages/purescript-profunctor-lenses>
  | Lenses
  | Prisms
  | Custom (CustomInstance lang)
  deriving (Eq, Show)

type PSInstance = Instance 'PureScript

data InstanceMember (lang :: Language) = InstanceMember
  { _memberName         :: Text
  , _memberBindings     :: [Text]
  , _memberBody         :: Text
  , _memberDependencies :: [TypeInfo lang]
  , _memberImportLines  :: ImportLines
  }
  deriving (Eq, Ord, Show)

data InstanceImplementation (lang :: Language)
  = Derive
  | DeriveNewtype
  | Explicit [InstanceMember lang]
  deriving (Eq, Ord, Show)

data CustomInstance (lang :: Language) = CustomInstance
  { _customConstraints    :: [TypeInfo lang]
  , _customHead           :: TypeInfo lang
  , _customImplementation :: InstanceImplementation lang
  }
  deriving (Eq, Ord, Show)

{- | The Purescript typeclass `Newtype` might be derivable if the original
Haskell type was a simple type wrapper.
-}
nootype :: [DataConstructor lang] -> Maybe (Instance lang)
nootype [DataConstructor _ (Record _)]   = Just Newtype
nootype [DataConstructor _ (Normal [_])] = Just Newtype
nootype _                                = Nothing

-- | Ensure that aeson-compatible `EncodeJson` and `DecodeJson` instances are generated for your type.
argonautJson :: SumType t -> SumType t
argonautJson (SumType ti dc is) = SumType ti dc . nub $ EncodeJson : DecodeJson : is

-- | Ensure that a generic `Show` instance is generated for your type.
genericShow :: SumType t -> SumType t
genericShow (SumType ti dc is) = SumType ti dc . nub $ GenericShow : is

{- | Ensure that a functor instance is generated for your type. It it
your responsibility to ensure your type is a functor.
-}
functor :: SumType t -> SumType t
functor (SumType ti dc is) = SumType ti dc . nub $ Functor : is

-- | Ensure that an `Eq` instance is generated for your type.
equal :: SumType t -> SumType t
equal (SumType ti dc is) = SumType ti dc . nub $ Eq : is

-- | Ensure that an `Eq1` instance is generated for your type.
equal1 :: SumType t -> SumType t
equal1 (SumType ti dc is) = SumType ti dc . nub $ Eq1 : is

-- | Ensure that both `Eq` and `Ord` instances are generated for your type.
order :: SumType t -> SumType t
order (SumType ti dc is) = SumType ti dc . nub $ Eq : Ord : is

lenses :: SumType t -> SumType t
lenses (SumType ti dc is) = SumType ti dc . nub $ Lenses : is

prisms :: SumType t -> SumType t
prisms (SumType ti dc is) = SumType ti dc . nub $ Prisms : is

data DataConstructor (lang :: Language) = DataConstructor
  { _sigConstructor :: !Text
    -- ^ e.g. `Left`/`Right` for `Either`
  , _sigValues      :: !(DataConstructorArgs lang)
  }
  deriving (Eq, Show)

data DataConstructorArgs (lang :: Language)
  = Nullary
  | Normal (NonEmpty (TypeInfo lang))
  | Record (NonEmpty (RecordEntry lang))
  deriving (Eq, Show)

instance Semigroup (DataConstructorArgs lang) where
    Nullary <> b           = b
    a <> Nullary           = a
    Normal as <> Normal bs = Normal $ as <> bs
    Record as <> Record bs = Record $ as <> bs
    Normal as <> Record bs = Normal as <> Normal (_recValue <$> bs)
    Record as <> Normal bs = Normal (_recValue <$> as) <> Normal bs

instance Monoid (DataConstructorArgs lang) where
    mempty = Nullary

data RecordEntry (lang :: Language) = RecordEntry
  { _recLabel :: !Text
    -- ^ e.g. `runState` for `State`
  , _recValue :: !(TypeInfo lang)
  }
  deriving (Eq, Show)

class GDataConstructor f where
    gToConstructors :: f a -> [DataConstructor 'Haskell]

class GDataConstructorArgs f where
    gToDataConstructorArgs :: f a -> DataConstructorArgs 'Haskell

instance (Datatype a, GDataConstructor c) => GDataConstructor (D1 a c) where
    gToConstructors (M1 c) = gToConstructors c

instance (GDataConstructor a, GDataConstructor b) => GDataConstructor (a :+: b) where
    gToConstructors _ =
        gToConstructors (undefined :: a f) ++ gToConstructors (undefined :: b f)

instance (Constructor a, GDataConstructorArgs b) => GDataConstructor (C1 a b) where
    gToConstructors c@(M1 r) =
        [ DataConstructor { _sigConstructor = constructor, _sigValues = values } ]
      where
        constructor = T.pack $ conName c
        values = gToDataConstructorArgs r

instance (GDataConstructorArgs a, GDataConstructorArgs b) => GDataConstructorArgs (a :*: b) where
    gToDataConstructorArgs _ =
        gToDataConstructorArgs (undefined :: a f) <> gToDataConstructorArgs (undefined :: b f)

instance GDataConstructorArgs U1 where
    gToDataConstructorArgs _ = mempty

instance (Selector a, Typeable t) => GDataConstructorArgs (S1 a (K1 R t)) where
    gToDataConstructorArgs e = case selName e of
        ""   -> Normal [mkTypeInfo @t]
        name -> Record [RecordEntry (T.pack name) (mkTypeInfo @t)]

{- | Get all used types in a sum type.

  This includes all types found at the right hand side of a sum type
  definition, not the type parameters of the sum type itself
-}
getUsedTypes :: SumType lang -> Set (TypeInfo lang)
getUsedTypes (SumType _ cs is) =
    Set.fromList . concatMap flattenTypeInfo $
        concatMap constructorToTypes cs <> concatMap instanceToTypes is

constructorToTypes :: DataConstructor lang -> [TypeInfo lang]
constructorToTypes (DataConstructor _ Nullary)     = []
constructorToTypes (DataConstructor _ (Normal ts)) = NE.toList ts
constructorToTypes (DataConstructor _ (Record rs)) = _recValue <$> NE.toList rs

instanceToTypes :: Instance lang -> [TypeInfo lang]
instanceToTypes Generic = pure . constraintToType $ TypeInfo "purescript-prelude" "Data.Generic.Rep" "Generic" []
instanceToTypes GenericShow = pure . constraintToType $ TypeInfo "purescript-prelude" "Prelude" "Show" []
instanceToTypes EncodeJson =
    pure . constraintToType $ TypeInfo "purescript-argonaut-codecs" "Data.Argonaut.Encode" "EncodeJson" []
instanceToTypes DecodeJson =
    pure . constraintToType $ TypeInfo "purescript-argonaut-codecs" "Data.Argonaut.Decode" "DecodeJson" []
instanceToTypes (ForeignObject _ _) = fmap constraintToType
    [ TypeInfo "purescript-foreign" "Foreign" "Foreign" []
    , TypeInfo "purescript-foreign-object" "Foreign.Object" "Object" []
    -- , TypeInfo "purescript-foreign-generic" "Foreign.Generic" "" []
    ]
instanceToTypes Newtype =
    pure . constraintToType $ TypeInfo "purescript-newtype" "Data.Newtype" "Newtype" []
instanceToTypes Functor =
    pure . constraintToType $ TypeInfo "purescript-prelude" "Prelude" "Functor" []
instanceToTypes Eq =
    pure . constraintToType $ TypeInfo "purescript-prelude" "Prelude" "Eq" []
instanceToTypes Eq1 =
    pure . constraintToType $ TypeInfo "purescript-prelude" "Data.Eq" "Eq1" []
instanceToTypes Ord =
    pure . constraintToType $ TypeInfo "purescript-prelude" "Prelude" "Ord" []
instanceToTypes Enum =
    pure . constraintToType $ TypeInfo "purescript-enums" "Data.Enum" "Enum" []
instanceToTypes Bounded =
    pure . constraintToType $ TypeInfo "purescript-prelude" "Prelude" "Bounded" []
instanceToTypes (Custom CustomInstance {..}) =
    constraintToType _customHead : (fmap constraintToType _customConstraints <> implementationToTypes _customImplementation)
instanceToTypes Lenses = mempty
instanceToTypes Prisms = mempty

constraintToType :: TypeInfo lang -> TypeInfo lang
constraintToType = over typeName ("class " <>)

implementationToTypes :: InstanceImplementation lang -> [TypeInfo lang]
implementationToTypes (Explicit members) = concatMap _memberDependencies members
implementationToTypes _                  = []

instanceToImportLines :: PSInstance -> ImportLines
instanceToImportLines GenericShow =
    importsFromList [ImportLine "Data.Show.Generic" Nothing $ Set.singleton "genericShow"]
instanceToImportLines EncodeJson =
    importsFromList
        [ ImportLine "Data.Argonaut.Aeson.Encode.Generic" Nothing
          $ Set.fromList ["genericEncodeAeson"]
        , ImportLine "Data.Argonaut.Aeson.Options" (Just "Argonaut")
          $ Set.fromList ["defaultOptions"]
        , ImportLine "Data.Argonaut.Encode.Class" (Just "Argonaut") mempty
        , ImportLine "Data.Argonaut.Encode.Class" Nothing
          $ Set.fromList ["class EncodeJson", "encodeJson"]
        ]
instanceToImportLines DecodeJson =
    importsFromList
        [ ImportLine "Data.Argonaut.Aeson.Decode.Generic" Nothing
          $ Set.fromList ["genericDecodeAeson"]
        , ImportLine "Data.Argonaut.Aeson.Options" (Just "Argonaut")
          $ Set.fromList ["defaultOptions"]
        , ImportLine "Data.Argonaut.Decode.Class" (Just "Argonaut") mempty
        , ImportLine "Data.Argonaut.Decode.Class" Nothing
          $ Set.fromList ["class DecodeJson", "class DecodeJsonField", "decodeJson"]
        ]
-- for IOHK library
-- This relies on unpublished Purescript library `purescript-bridge-json-helpers`:
-- https://github.com/input-output-hk/purescript-bridge-json-helpers
-- and `purescript-argonaut-codecs`
-- https://pursuit.purescript.org/packages/purescript-argonaut-codecs
-- instanceToImportLines EncodeJson =
--     importsFromList
--         [ ImportLine "Control.Lazy" Nothing $ Set.singleton "defer"
--         , ImportLine "Data.Argonaut" Nothing $ Set.fromList ["encodeJson", "jsonNull"]
--         , ImportLine "Data.Argonaut.Encode.Aeson" Nothing $ Set.fromList ["(>$<)", "(>/\\<)"]
--         , ImportLine "Data.Newtype" Nothing $ Set.singleton "unwrap"
--         , ImportLine "Data.Tuple.Nested" Nothing $ Set.singleton "(/\\)"
--         ]
-- instanceToImportLines DecodeJson =
--     importsFromList
--         [ ImportLine "Control.Lazy" Nothing $ Set.singleton "defer"
--         , ImportLine "Data.Argonaut.Decode.Aeson" Nothing $ Set.fromList ["(</$\\>)", "(</*\\>)", "(</\\>)"]
--         , ImportLine "Data.Newtype" Nothing $ Set.singleton "unwrap"
--         , ImportLine "Data.Tuple.Nested" Nothing $ Set.singleton "(/\\)"
--         ]
instanceToImportLines (ForeignObject _ _) =
    importsFromList
        [ ImportLine "Foreign.Class" Nothing
          $ Set.fromList ["class Decode", "class Encode"]
        , ImportLine "Foreign.Generic" Nothing
          $ Set.fromList ["defaultOptions", "genericDecode", "genericEncode"]
        ]
instanceToImportLines Enum =
    importsFromList
        [ ImportLine "Data.Enum.Generic" Nothing $ Set.fromList ["genericPred", "genericSucc"]
        ]
instanceToImportLines Bounded =
    importsFromList
        [ ImportLine "Data.Bounded.Generic" Nothing $ Set.fromList ["genericBottom", "genericTop"]
        ]
instanceToImportLines (Custom CustomInstance {_customImplementation = Explicit members}) =
    importsFromList $ concatMap (Map.elems . _memberImportLines) members
instanceToImportLines Lenses =
    importsFromList
        [ ImportLine "Data.Lens" Nothing $ Set.fromList
            [ "Iso'"
            , "Lens'"
            , "Prism'"
            , "iso"
            , "lens"
            , "prism'"
            ]
        ]
instanceToImportLines Prisms = instanceToImportLines Prisms
instanceToImportLines (Custom _) = mempty
instanceToImportLines Generic = mempty
instanceToImportLines Newtype =
    importsFromList
        [ ImportLine "Data.Lens.Iso.Newtype" Nothing $ Set.fromList ["_Newtype"]
        , ImportLine "Data.Lens.Record" Nothing $ Set.fromList ["prop"]
        , ImportLine "Type.Proxy" Nothing $ Set.fromList ["Proxy(Proxy)"]
        , ImportLine "Data.Newtype" Nothing $ Set.fromList ["class Newtype"]
        ]
instanceToImportLines Functor = mempty
instanceToImportLines Eq = mempty
instanceToImportLines Eq1 = mempty
instanceToImportLines Ord = mempty

importsFromList :: [ImportLine] -> Map Text ImportLine
importsFromList ls =
    let pairs = zip (importModule <$> ls) ls
        merge a b =
            ImportLine (importModule a) Nothing (importTypes a `Set.union` importTypes b)
     in Map.fromListWith merge pairs

-- Lenses:
makeLenses ''DataConstructor

makeLenses ''RecordEntry

makeLenses ''CustomInstance

makeLenses ''InstanceImplementation

makeLenses ''InstanceMember
