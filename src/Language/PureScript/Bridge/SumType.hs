{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}

module Language.PureScript.Bridge.SumType
    ( SumType (..)
    , mkSumType
    , mkSumTypeWith
    , equal
    , order 
    , DataConstructor (..)
    , DataConstructorOpts (..)
    , defaultDataConstructorOpts
    , RecordEntry (..)
    , Instance (..)
    , nootype
    , getUsedTypes
    , constructorToTypes
    , sigConstructor
    , sigValues
    , sumTypeInfo
    , sumTypeConstructors
    , recLabel
    , recValue
    , recLabelModifier
    ) where

import           Control.Lens hiding (from, to)
import           Data.List (nub)
import           Data.Maybe (maybeToList)
import           Data.Proxy
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable
import           Generics.Deriving

import           Language.PureScript.Bridge.TypeInfo

-- | Generic representation of your Haskell types.
data SumType (lang :: Language) = SumType (TypeInfo lang) [DataConstructor lang] [Instance]
  deriving (Eq, Show)

-- | TypInfo lens for 'SumType'.
sumTypeInfo :: Functor f => (TypeInfo lang -> f (TypeInfo lang)) -> SumType lang -> f (SumType lang)
sumTypeInfo inj (SumType info constrs is) = (\ti -> SumType ti constrs is) <$> inj info

-- | DataConstructor lens for 'SumType'.
sumTypeConstructors :: Functor f => ([DataConstructor lang] -> f [DataConstructor lang]) -> SumType lang -> f (SumType lang)
sumTypeConstructors inj (SumType info constrs is) = (\cs -> SumType info cs is) <$> inj constrs

{- | Create a representation of your sum (and product) types,
  for doing type translations and writing it out to your PureScript modules.
  In order to get the type information we use a dummy variable of type 'Proxy' (YourType).
-}
mkSumType
    :: forall t
     . (Generic t, Typeable t, GDataConstructor (Rep t))
    => Proxy t
    -> SumType 'Haskell
mkSumType p = SumType (mkTypeInfo p) constructors (Encode : Decode : EncodeJson : DecodeJson : Generic : maybeToList (nootype constructors))
  where
    constructors = gToConstructors (from (undefined :: t))

mkSumTypeWith
    :: forall t
     . (Generic t, Typeable t, GDataConstructor (Rep t))
    => DataConstructorOpts
    -> Proxy t
    -> SumType 'Haskell
mkSumTypeWith opts p = SumType (mkTypeInfo p) constructors (Encode : Decode : EncodeJson : DecodeJson : Generic : maybeToList (nootype constructors))
  where
    constructors = gToConstructorsWithOpts opts (from (undefined :: t))

-- | Purescript typeclass instances that can be generated for your Haskell types.
data Instance = Encode | EncodeJson | Decode | DecodeJson | Generic | Newtype | Eq | Ord
  deriving (Eq, Show)

{- | The Purescript typeclass `Newtype` might be derivable if the original
Haskell type was a simple type wrapper.
-}
nootype :: [DataConstructor lang] -> Maybe Instance
nootype cs = case cs of
    [constr]
        | either isSingletonList (const True) (_sigValues constr) -> Just Newtype
        | otherwise -> Nothing
    _ -> Nothing
  where
    isSingletonList [_] = True
    isSingletonList _   = False

-- | Ensure that an `Eq` instance is generated for your type.
equal :: Eq a => Proxy a -> SumType t -> SumType t
equal _ (SumType ti dc is) = SumType ti dc . nub $ Eq : is

-- | Ensure that both `Eq` and `Ord` instances are generated for your type.
order :: Ord a => Proxy a -> SumType t -> SumType t
order _ (SumType ti dc is) = SumType ti dc . nub $ Eq : Ord : is

data DataConstructor (lang :: Language) = DataConstructor
  { _sigConstructor :: !Text
    -- ^ e.g. `Left`/`Right` for `Either`
  , _sigValues      :: !(Either [TypeInfo lang] [RecordEntry lang])
  }
  deriving (Eq, Show)

data RecordEntry (lang :: Language) = RecordEntry
  { _recLabel :: !Text
    -- ^ e.g. `runState` for `State`
  , _recValue :: !(TypeInfo lang)
  }
  deriving (Eq, Show)

newtype DataConstructorOpts = 
  DataConstructorOpts 
    { _recLabelModifier :: String -> String }

defaultDataConstructorOpts :: DataConstructorOpts
defaultDataConstructorOpts = 
  DataConstructorOpts 
    { _recLabelModifier = id }

class GDataConstructor f where
    gToConstructorsWithOpts :: DataConstructorOpts -> f a -> [DataConstructor 'Haskell]

    gToConstructors :: f a -> [DataConstructor 'Haskell]
    gToConstructors = gToConstructorsWithOpts defaultDataConstructorOpts

class GRecordEntry f where
    gToRecordEntriesWithOpts :: DataConstructorOpts -> f a -> [RecordEntry 'Haskell]

instance (Datatype a, GDataConstructor c) => GDataConstructor (D1 a c) where
    gToConstructorsWithOpts opts (M1 c) = gToConstructorsWithOpts opts c

instance (GDataConstructor a, GDataConstructor b) => GDataConstructor (a :+: b) where
    gToConstructorsWithOpts opts (_ :: (a :+: b) f) =
        gToConstructorsWithOpts opts (undefined :: a f) ++ gToConstructorsWithOpts opts (undefined :: b g)

instance (Constructor a, GRecordEntry b) => GDataConstructor (C1 a b) where
    gToConstructorsWithOpts opts c@(M1 r) =
        [ DataConstructor
            { _sigConstructor = constructor
            , _sigValues = values
            }
        ]
      where
        constructor = T.pack $ conName c
        values =
            if conIsRecord c
                then Right $ gToRecordEntriesWithOpts opts r
                else Left $ map _recValue $ gToRecordEntriesWithOpts opts r

instance (GRecordEntry a, GRecordEntry b) => GRecordEntry (a :*: b) where
    gToRecordEntriesWithOpts opts (_ :: (a :*: b) f) =
        gToRecordEntriesWithOpts opts (undefined :: a f)
            ++ gToRecordEntriesWithOpts opts (undefined :: b f)

instance GRecordEntry U1 where
    gToRecordEntriesWithOpts _ _ = []

instance (Selector a, Typeable t) => GRecordEntry (S1 a (K1 R t)) where
    gToRecordEntriesWithOpts opts e =
        [ RecordEntry
            { _recLabel = T.pack $ _recLabelModifier opts (selName e)
            , _recValue = mkTypeInfo (Proxy :: Proxy t)
            }
        ]

{- | Get all used types in a sum type.

  This includes all types found at the right hand side of a sum type
  definition, not the type parameters of the sum type itself
-}
getUsedTypes :: SumType lang -> Set (TypeInfo lang)
getUsedTypes (SumType _ cs _) = foldr constructorToTypes Set.empty cs

constructorToTypes :: DataConstructor lang -> Set (TypeInfo lang) -> Set (TypeInfo lang)
constructorToTypes (DataConstructor _ (Left myTs)) ts =
    Set.fromList (concatMap flattenTypeInfo myTs) `Set.union` ts
constructorToTypes (DataConstructor _ (Right rs)) ts =
    Set.fromList (concatMap (flattenTypeInfo . _recValue) rs) `Set.union` ts

-- Lenses:
makeLenses ''DataConstructor
makeLenses ''RecordEntry
makeLenses ''DataConstructorOpts

