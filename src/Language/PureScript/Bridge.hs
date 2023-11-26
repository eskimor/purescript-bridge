{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Language.PureScript.Bridge
    ( bridgeSumType
    , defaultBridge
    , module Bridge
    , writePSTypes
    , writePSTypesWith
    , writePSTypesWithNamespace
    ) where

import           Control.Applicative (Alternative ((<|>)))
import           Control.Lens (over, traversed)
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Text.IO as T
import           Language.PureScript.Bridge.Builder as Bridge (BridgeBuilder,
                                                               BridgeData,
                                                               BridgePart,
                                                               FixUpBridge,
                                                               FixUpBuilder,
                                                               FullBridge,
                                                               buildBridge,
                                                               buildBridgeWithCustomFixUp,
                                                               clearPackageFixUp,
                                                               doCheck,
                                                               errorFixUp,
                                                               fullBridge,
                                                               psTypeParameters,
                                                               (<|>), (^==))
import           Language.PureScript.Bridge.Primitives as Bridge (boolBridge,
                                                                  doubleBridge,
                                                                  dummyBridge,
                                                                  eitherBridge,
                                                                  intBridge,
                                                                  listBridge,
                                                                  mapBridge,
                                                                  maybeBridge,
                                                                  noContentBridge,
                                                                  setBridge,
                                                                  strMapBridge,
                                                                  stringBridge,
                                                                  textBridge,
                                                                  unitBridge,
                                                                  word16Bridge,
                                                                  word32Bridge,
                                                                  word64Bridge,
                                                                  word8Bridge,
                                                                  wordBridge)
import           Language.PureScript.Bridge.Printer as Bridge (Module (..),
                                                               Modules,
                                                               PSModule,
                                                               PackageName (..),
                                                               branch, caseOf,
                                                               case_of,
                                                               constrainWith,
                                                               constructor,
                                                               constructorOptics,
                                                               constructorPattern,
                                                               constructorToDecode,
                                                               constructorToDoc,
                                                               constructorToOptic,
                                                               decodeJsonConstraints,
                                                               def, encloseHsep,
                                                               encloseVsep,
                                                               encodeJsonConstraints,
                                                               eqConstraints,
                                                               field,
                                                               fieldSignature,
                                                               fieldSignatures,
                                                               fields,
                                                               flattenTuple,
                                                               fromEntries,
                                                               hasUnderscore,
                                                               hrecord,
                                                               importLineToText,
                                                               instanceToQualifiedImports,
                                                               instances,
                                                               instancesToImportLines,
                                                               instancesToQualifiedImports,
                                                               isEnum,
                                                               isTypeParam, iso,
                                                               lambda,
                                                               memberToMethod,
                                                               mkFnArgs,
                                                               mkPackageName,
                                                               mkType,
                                                               moduleToText,
                                                               newtypeIso,
                                                               normalExpr,
                                                               normalLabels,
                                                               normalPattern,
                                                               nullaryExpr,
                                                               nullaryPattern,
                                                               ordConstraints,
                                                               pattern,
                                                               printModule,
                                                               prism,
                                                               qualifiedImportToText,
                                                               recordEntryToLens,
                                                               recordOptics,
                                                               recordPattern,
                                                               renderText,
                                                               showConstraints,
                                                               signature,
                                                               signature',
                                                               spaces,
                                                               sumTypeToDecode,
                                                               sumTypeToDocs,
                                                               sumTypeToEncode,
                                                               sumTypeToModule,
                                                               sumTypeToNeededPackages,
                                                               sumTypeToOptics,
                                                               sumTypeToTypeDecls,
                                                               sumTypesToModules,
                                                               sumTypesToNeededPackages,
                                                               typeInfoToDecl,
                                                               typeInfoToDoc,
                                                               typeParams,
                                                               typeToDecode,
                                                               typeToEncode,
                                                               typeToImportLines,
                                                               typesToImportLines,
                                                               typesToRecord,
                                                               unionImportLine,
                                                               unionImportLines,
                                                               unionModules,
                                                               unionQualifiedImports,
                                                               unlessM, vrecord)
import           Language.PureScript.Bridge.SumType as Bridge (CustomInstance (..),
                                                               DataConstructor (..),
                                                               DataConstructorArgs (..),
                                                               GDataConstructor,
                                                               ImportLine (..),
                                                               ImportLines,
                                                               Instance (..),
                                                               InstanceImplementation (..),
                                                               InstanceMember (..),
                                                               PSInstance,
                                                               RecordEntry (..),
                                                               SumType (..),
                                                               argonautAesonGeneric,
                                                               baselineImports,
                                                               constructorToTypes,
                                                               customConstraints,
                                                               customHead,
                                                               customImplementation,
                                                               equal, equal1,
                                                               functor,
                                                               genericShow,
                                                               getUsedTypes,
                                                               importsFromList,
                                                               instanceToImportLines,
                                                               jsonHelper,
                                                               lenses,
                                                               memberBindings,
                                                               memberBody,
                                                               memberDependencies,
                                                               memberImportLines,
                                                               memberName,
                                                               mkSumType,
                                                               nootype, order,
                                                               prisms, recLabel,
                                                               recValue,
                                                               sigConstructor,
                                                               sigValues,
                                                               sumTypeConstructors,
                                                               sumTypeInfo)
import           Language.PureScript.Bridge.Tuple as Bridge (TupleParserState (..),
                                                             isTuple, step,
                                                             tupleBridge)
import           Language.PureScript.Bridge.TypeInfo as Bridge (HasHaskType (..),
                                                                HaskellType,
                                                                Language (..),
                                                                PSType,
                                                                TypeInfo (..),
                                                                flattenTypeInfo,
                                                                mkTypeInfo,
                                                                mkTypeInfo',
                                                                typeModule,
                                                                typeName,
                                                                typePackage,
                                                                typeParameters)

{- | Your entry point to this library and quite likely all you will need.
  Make sure all your types derive `Generic` and `Typeable`.
  Typeable is not needed from ghc-7.10 on.

  Then list all your types you want to use in PureScript and call 'writePSTypes':

  > data Foo = Foo { ... } deriving (Eq, Generic)
  > data Bar = A | B | C deriving (Eq, Ord, Generic)
  > data Baz = ... deriving (Generic)
  >
  > -- | All types will have a `Generic` instance produced in Purescript.
  > myTypes :: [SumType 'Haskell]
  > myTypes =
  >   [ equal (mkSumType @Foo)  -- Also produce a `Eq` instance.
  >   , order (mkSumType @Bar)  -- Produce both `Eq` and `Ord`.
  >   , mkSumType @Baz  -- Just produce a `Generic` instance.
  >   ]
  >
  >  writePSTypes "path/to/your/purescript/project" (buildBridge defaultBridge) myTypes

  You can define your own type bridges based on 'defaultBridge':


 >  myBridge = defaultBridge <|> mySpecialTypeBridge

 and use it with 'writePSTypes':

 >  writePSTypes "path/to/your/purescript/project" (buildBridge myBridge) myTypes

  Find examples for implementing your own bridges in: "Language.PureScript.Bridge.Primitives".

 == Result:
  'writePSTypes' will write out PureScript modules to the given path, mirroring the hierarchy of the Haskell modules
  the types came from. In addition a list of needed PS packages is printed to the console.

  The list of needed packages is retrieved from the bridged 'TypeInfo' data, so make sure you set '_typePackage' correctly
  in your own bridges, in order for this feature to be useful.

 == Real world usage example (at time of this writing outdated, at time of reading hopefully fixed):
  A real world use case of this library can be found <https://github.com/gonimo/gonimo-back/blob/master/app/PSGenerator.hs here>.

  With custom bridges defined <https://github.com/gonimo/gonimo-back/blob/master/src/Gonimo/CodeGen/TypeBridges.hs here> and
  custom PS types defined <https://github.com/gonimo/gonimo-back/blob/master/src/Gonimo/CodeGen/PSTypes.hs here>.

  Parts of the generated output can be found <https://github.com/gonimo/gonimo-front/blob/master/src/Gonimo/Types.purs here>.

  Note how 'Secret' and 'Key'
  get translated according to our custom rules, with correct imports and everything.
  Also the formatting is quite nice, would you have guessed that this code was generated?

 == /WARNING/:
  This function overwrites files - make backups or use version control!
-}
writePSTypes :: FilePath -> FullBridge -> [SumType 'Haskell] -> IO ()
writePSTypes = writePSTypesWith

{- | Works like `writePSTypes` but you can add additional switches to control the generation of your PureScript code

 == Switches/Settings:

  - `noLenses` and `genLenses` to control if the `purescript-profunctor-lenses` are generated for your types

 == /WARNING/:
  This function overwrites files - make backups or use version control!
-}
writePSTypesWith :: FilePath -> FullBridge -> [SumType 'Haskell] -> IO ()
writePSTypesWith = writePSTypesWithNamespace Nothing

writePSTypesWithNamespace
    :: Maybe PackageName -> FilePath -> FullBridge -> [SumType 'Haskell] -> IO ()
writePSTypesWithNamespace packageName root bridge sts = do
    mapM_ (printModule root) modules
    T.putStrLn
        "The following purescript packages are needed by the generated code:\n"
    mapM_ (T.putStrLn . mappend "  - ") packages
    T.putStrLn "\nSuccessfully created your PureScript modules!"
  where
    bridged = map (bridgeSumType bridge) sts
    modules = M.elems $ sumTypesToModules packageName bridged
    packages =
        sumTypesToNeededPackages bridged
            <> Set.singleton "purescript-profunctor-lenses"

{- | Translate all 'TypeInfo' values in a 'SumType' to PureScript types.

  Example usage, with defaultBridge:

> data Foo = Foo | Bar Int | FooBar Int Text deriving (Generic, Typeable, Show)

> bridgeSumType (buildBridge defaultBridge) (mkSumType @Foo)
-}
bridgeSumType :: FullBridge -> SumType 'Haskell -> SumType 'PureScript
bridgeSumType br (SumType t cs is) =
    SumType (br t) (map (bridgeConstructor br) cs) $ bridgeInstance <$> (is <> extraInstances)
  where
    bridgeInstance (Custom CustomInstance {..}) =
        Custom $
            CustomInstance
                (br <$> _customConstraints)
                (br _customHead)
                case _customImplementation of
                    Derive           -> Derive
                    DeriveNewtype    -> DeriveNewtype
                    Explicit members -> Explicit $ bridgeMember <$> members
    bridgeInstance Bounded = Bounded
    bridgeInstance Enum = Enum
    bridgeInstance EncodeJson = EncodeJson
    bridgeInstance DecodeJson = DecodeJson
    bridgeInstance EncodeJsonHelper = EncodeJsonHelper
    bridgeInstance DecodeJsonHelper = DecodeJsonHelper
    bridgeInstance (ForeignObject x y) = ForeignObject x y
    bridgeInstance GenericShow = GenericShow
    bridgeInstance Functor = Functor
    bridgeInstance Eq = Eq
    bridgeInstance Eq1 = Eq1
    bridgeInstance Ord = Ord
    bridgeInstance Generic = Generic
    bridgeInstance Newtype = Newtype
    bridgeInstance Lenses = Lenses
    bridgeInstance Prisms = Prisms
    bridgeMember = over (memberDependencies . traversed) br
    extraInstances
        | not (null cs) && all isNullary cs = [Enum, Bounded]
        | otherwise = []
    isNullary (DataConstructor _ args) = args == Nullary

{- | Default bridge for mapping primitive/common types:
  You can append your own bridges like this:

>  defaultBridge <|> myBridge1 <|> myBridge2

  Find examples for bridge definitions in "Language.PureScript.Bridge.Primitives" and
  "Language.PureScript.Bridge.Tuple".
-}
defaultBridge :: BridgePart
defaultBridge =
    textBridge
        <|> stringBridge
        <|> listBridge
        <|> maybeBridge
        <|> eitherBridge
        <|> strMapBridge
        <|> boolBridge
        <|> intBridge
        <|> doubleBridge
        <|> tupleBridge
        <|> unitBridge
        <|> mapBridge
        <|> setBridge
        <|> noContentBridge
        <|> wordBridge
        <|> word8Bridge
        <|> word16Bridge
        <|> word32Bridge
        <|> word64Bridge

-- | Translate types in a constructor.
bridgeConstructor
    :: FullBridge -> DataConstructor 'Haskell -> DataConstructor 'PureScript
bridgeConstructor _ (DataConstructor name Nullary) =
    DataConstructor name Nullary
bridgeConstructor br (DataConstructor name (Normal infos)) =
    DataConstructor name . Normal $ fmap br infos
bridgeConstructor br (DataConstructor name (Record record)) =
    DataConstructor name . Record $ fmap (bridgeRecordEntry br) record

-- | Translate types in a record entry.
bridgeRecordEntry
    :: FullBridge -> RecordEntry 'Haskell -> RecordEntry 'PureScript
bridgeRecordEntry br (RecordEntry label value) = RecordEntry label $ br value
