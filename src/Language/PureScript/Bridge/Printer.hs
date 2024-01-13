{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Language.PureScript.Bridge.Printer where

import           Control.Arrow ((&&&))
import           Control.Lens (to, (%~), (<>~), (^.))
import           Control.Monad (unless)
import           Data.Char (isLower)
import qualified Data.Char as C
import           Data.Function (on, (&))
import           Data.List (groupBy, nubBy, sortBy)
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes, fromMaybe, isJust)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Language.PureScript.Bridge.PSTypes (psUnit)
import           Language.PureScript.Bridge.SumType (CustomInstance (..),
                                                     DataConstructor (..),
                                                     DataConstructorArgs (..),
                                                     ImportLine (..),
                                                     ImportLines, Instance (..),
                                                     InstanceImplementation (..),
                                                     InstanceMember (..),
                                                     PSInstance,
                                                     RecordEntry (..),
                                                     SumType (SumType),
                                                     _recLabel, baselineImports,
                                                     getUsedTypes,
                                                     importsFromList,
                                                     instanceToImportLines,
                                                     nootype, recLabel,
                                                     recValue, sigConstructor)
import           Language.PureScript.Bridge.TypeInfo (Language (PureScript),
                                                      PSType,
                                                      TypeInfo (TypeInfo),
                                                      _typeModule, _typeName,
                                                      _typePackage,
                                                      _typeParameters,
                                                      flattenTypeInfo, typeName)
import           System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import           System.FilePath (joinPath, takeDirectory, (</>))
import           Text.PrettyPrint.Leijen.Text (Doc, backslash, char, colon,
                                               comma, displayTStrict, dquotes,
                                               hang, hsep, indent, lbrace,
                                               lbracket, line, linebreak,
                                               lparen, nest, parens, punctuate,
                                               rbrace, rbracket, renderPretty,
                                               rparen, softline, textStrict,
                                               vsep, (<+>))

renderText :: Doc -> Text
renderText = T.replace " \n" "\n" . displayTStrict . renderPretty 0.4 200

data Module (lang :: Language) = PSModule
  { psModuleName       :: !Text
  , psImportLines      :: !ImportLines
  , psQualifiedImports :: !(Map Text Text)
  , psTypes            :: ![SumType lang]
  }
  deriving (Show)

type PSModule = Module 'PureScript

type Modules = Map Text PSModule

newtype PackageName
  = PackageName Text

mkPackageName :: String -> Maybe PackageName
mkPackageName s =
    if all C.isAlpha s
        then Just $ PackageName (T.pack s)
        else Nothing

sumTypesToModules :: Maybe PackageName -> [SumType 'PureScript] -> Modules
sumTypesToModules packageName =
    foldr (Map.unionWith unionModules) Map.empty . fmap (sumTypeToModule packageName)

unionModules :: PSModule -> PSModule -> PSModule
unionModules m1 m2 =
    m1
        { psImportLines = unionImportLines (psImportLines m1) (psImportLines m2)
        , psTypes = psTypes m1 <> psTypes m2
        }

sumTypeToModule :: Maybe PackageName -> SumType 'PureScript -> Modules
sumTypeToModule packageName sumType@(SumType typeInfo _ stInstances) =
    Map.singleton
        typedModuleName
        $ PSModule
            { psModuleName = psModuleName
            , psImportLines = dropEmpty
              . dropPrelude
              . dropPrim
              . dropSelf
              $ unionImportLines
                  (typesToImportLines (getUsedTypes sumType))
                  (instancesToImportLines stInstances <> baselineImports)
            , psQualifiedImports = instancesToQualifiedImports stInstances
            , psTypes = [sumType]
            }
  where
    dropEmpty = Map.delete ""
    dropPrelude = Map.delete "Prelude"
    dropPrim = Map.delete "Prim"
    typedModuleName = _typeModule typeInfo
    dropSelf = Map.delete typedModuleName
    psModuleName = fromMaybe typedModuleName do
        PackageName pn <- packageName
        pure $ pn <> "." <> typedModuleName

unionQualifiedImports :: Map Text Text -> Map Text Text -> Map Text Text
unionQualifiedImports = Map.unionWith const

unionImportLines :: ImportLines -> ImportLines -> ImportLines
unionImportLines = Map.unionWith unionImportLine

unionImportLine :: ImportLine -> ImportLine -> ImportLine
unionImportLine l1 l2 =
    l1 {importTypes = Set.union (importTypes l1) (importTypes l2)}

typesToImportLines :: Set PSType -> ImportLines
typesToImportLines =
    foldr unionImportLines Map.empty . fmap typeToImportLines . Set.toList

typeToImportLines :: PSType -> ImportLines
typeToImportLines t =
    unionImportLines (typesToImportLines $ Set.fromList (_typeParameters t)) $
        importsFromList [ImportLine (_typeModule t) Nothing (Set.singleton (_typeName t))]

instancesToQualifiedImports :: [PSInstance] -> Map Text Text
instancesToQualifiedImports =
    foldr unionQualifiedImports Map.empty . fmap instanceToQualifiedImports

instancesToImportLines :: [PSInstance] -> ImportLines
instancesToImportLines =
    foldr unionImportLines Map.empty . fmap instanceToImportLines

instanceToQualifiedImports :: PSInstance -> Map Text Text
instanceToQualifiedImports _ = Map.empty

printModule :: FilePath -> PSModule -> IO ()
printModule root m = do
    unlessM (doesDirectoryExist mDir) $ createDirectoryIfMissing True mDir
    T.writeFile mPath . moduleToText $ m
  where
    mFile = (joinPath . map T.unpack . T.splitOn "." $ psModuleName m) <> ".purs"
    mPath = root </> mFile
    mDir = takeDirectory mPath

sumTypesToNeededPackages :: [SumType lang] -> Set Text
sumTypesToNeededPackages = Set.unions . map sumTypeToNeededPackages

sumTypeToNeededPackages :: SumType lang -> Set Text
sumTypeToNeededPackages st =
    Set.filter (not . T.null) . Set.map _typePackage $ getUsedTypes st

moduleToText :: Module 'PureScript -> Text
moduleToText m =
    flip mappend "\n" $
        renderText $
            vsep $
                [ "-- File auto generated by purescript-bridge! --"
                , "module" <+> textStrict (psModuleName m) <+> "where" <> linebreak
                , "import Prelude" <> linebreak
                , vsep
                    ( (importLineToText <$> allImports)
                        <> (uncurry qualifiedImportToText <$> Map.toList (psQualifiedImports m))
                    )
                    <> linebreak
                ]
                    <> punctuate (line <> line <> dashes <> line) (sumTypeToDocs =<< psTypes m)
  where
    allImports = Map.elems $ psImportLines m
    dashes = textStrict (T.replicate 80 "-")

qualifiedImportToText :: Text -> Text -> Doc
qualifiedImportToText m q = hsep ["import", textStrict m, "as", textStrict q]

importLineToText :: ImportLine -> Doc
importLineToText l =
  let
    typeListDoc = if null typeList
      then mempty
      else encloseHsep lparen rparen comma typeList
  in case importAlias l of
    Just alias ->
      hsep ["import", textStrict $ importModule l, typeListDoc, "as", textStrict alias]
    Nothing ->
      hsep ["import", textStrict $ importModule l, typeListDoc]
  where
    typeList =
        map (textStrict . last)
            . groupBy ((==) `on` importedType)
            . sortBy importOrder
            . Set.toList
            $ importTypes l
    importOrder imp1 imp2
        | T.isPrefixOf "class" imp1 = if T.isPrefixOf "class" imp2 then compare imp1 imp2 else LT
        | otherwise = compare imp1 imp2
    importedType imp = fromMaybe imp $ T.stripSuffix "(..)" imp

sumTypeToDocs :: SumType 'PureScript -> [Doc]
sumTypeToDocs st@(SumType _ _ is) | (elem Prisms is) = [sumTypeToTypeDecls st, sumTypeToOptics st]
sumTypeToDocs st@(SumType _ _ is) | (elem Lenses is) = [sumTypeToTypeDecls st, sumTypeToOptics st]
sumTypeToDocs st = [sumTypeToTypeDecls st]

sumTypeToTypeDecls :: SumType 'PureScript -> Doc
sumTypeToTypeDecls st@(SumType t cs _) =
    vsep $ punctuate line $ typeDecl : instances st
  where
    typeDecl
        | isJust (nootype cs) = mkTypeDecl "newtype"
        | otherwise = mkTypeDecl "data"
    mkTypeDecl keyword =
        keyword <+> typeInfoToDecl t <+> encloseVsep "=" mempty "|" (constructorToDoc <$> cs)

typeInfoToDecl :: PSType -> Doc
typeInfoToDecl (TypeInfo _ _ name params) =
    hsep $ textStrict name : (typeInfoToDoc <$> params)

typeInfoToDoc :: PSType -> Doc
typeInfoToDoc t@(TypeInfo _ _ _ params) =
    (if null params then id else parens) $ typeInfoToDecl t

constructorToDoc :: DataConstructor 'PureScript -> Doc
constructorToDoc (DataConstructor n args) =
    hsep $
        textStrict n : case args of
            Nullary   -> []
            Normal ts -> NE.toList $ typeInfoToDoc <$> ts
            Record rs -> [vrecord $ fieldSignatures rs]

{- | Given a PureScript type, generate instances for typeclass instances it claims to have.
-}
instances :: SumType 'PureScript -> [Doc]
instances st@(SumType t _ is) = go <$> is
  where
    mkConstraints :: (PSType -> [PSType]) -> [Doc]
    mkConstraints getConstraints = case getConstraints t of
        [] -> []
        constraints -> [encloseHsep lparen rparen comma (typeInfoToDecl <$> constraints), "=>"]
    mkInstance instanceHead getConstraints methods =
        vsep
            [ hsep
                [ "instance"
                , hsep $ mkConstraints getConstraints <> [typeInfoToDecl instanceHead]
                , "where"
                ]
            , indent 2 $ vsep methods
            ]
    mkDerivedInstance instanceHead getConstraints =
        hsep
            [ "derive instance"
            , hsep $ mkConstraints getConstraints <> [typeInfoToDecl instanceHead]
            ]
    mkDerivedNewtypeInstance instanceHead getConstraints =
        hsep
            [ "derive newtype instance"
            , hsep $ mkConstraints getConstraints <> [typeInfoToDecl instanceHead]
            ]
    toKind1 (TypeInfo p m n []) = TypeInfo p m n []
    toKind1 (TypeInfo p m n ps) = TypeInfo p m n $ init ps
    go :: PSInstance -> Doc
    go (Custom CustomInstance {..}) = case _customImplementation of
        Derive -> mkDerivedInstance _customHead (const _customConstraints)
        DeriveNewtype -> mkDerivedNewtypeInstance _customHead (const _customConstraints)
        Explicit members -> mkInstance _customHead (const _customConstraints) $ memberToMethod <$> members
    go Bounded =
        mkInstance
            (mkType "Bounded" [t])
            (const [])
            [ "bottom = genericBottom"
            , "top = genericTop"
            ]
    go Enum =
        mkInstance
            (mkType "Enum" [t])
            (const [])
            [ "succ = genericSucc"
            , "pred = genericPred"
            ]

    -- TODO
    -- render ForeignObject instances with these configuration
    -- " { unwrapSingleConstructors = "
    --     <> (T.toLower . T.pack . show $ unwrapSingleConstructors)
    --     <> " , unwrapSingleArguments = "
    --     <> (T.toLower . T.pack . show $ unwrapSingleArguments)
    --     <> " }"
    go (ForeignObject _ _) = go Generic
    -- This relies on `purescript-argonaut-aeson-generic`:
    -- https://pursuit.purescript.org/packages/purescript-argonaut-aeson-generic
    go EncodeJson =
        mkInstance
            (mkType "EncodeJson" [t])
            encodeJsonConstraints
            ["encodeJson = defer \\_ -> genericEncodeAeson Argonaut.defaultOptions"]
    -- This relies on `purescript-argonaut-aeson-generic`:
    -- https://pursuit.purescript.org/packages/purescript-argonaut-aeson-generic
    go DecodeJson =
        mkInstance
            (mkType "DecodeJson" [t])
            decodeJsonConstraints
            ["decodeJson = defer \\_ -> genericDecodeAeson Argonaut.defaultOptions"]
    {-|
      This relies on unpublished PureScript library `purescript-bridge-json-helpers`:
      https://github.com/input-output-hk/purescript-bridge-json-helpers
      and `purescript-argonaut-codecs`
      https://pursuit.purescript.org/packages/purescript-argonaut-codecs
    -}
    go EncodeJsonHelper =
        vsep $
            punctuate
                line
                [ mkInstance
                    (mkType "EncodeJson" [t])
                    encodeJsonConstraints
                    ["encodeJson = defer \\_ ->" <+> sumTypeToEncode st]
                ]
    go DecodeJsonHelper =
        vsep $
            punctuate
                line
                [ mkInstance
                    (mkType "DecodeJson" [t])
                    decodeJsonConstraints
                    [hang 2 $ "decodeJson = defer \\_ -> D.decode" <+> sumTypeToDecode st]
                ]
    go GenericShow = mkInstance (mkType "Show" [t]) showConstraints ["show a = genericShow a"]
    go Functor = mkDerivedInstance (mkType "Functor" [toKind1 t]) (const [])
    go Eq = mkDerivedInstance (mkType "Eq" [t]) eqConstraints
    go Eq1 = mkDerivedInstance (mkType "Eq1" [toKind1 t]) (const [])
    go Ord = mkDerivedInstance (mkType "Ord" [t]) ordConstraints
    go Generic = mkDerivedInstance (mkType "Generic" [t, mkType "_" []]) (const [])
    go Newtype = mkDerivedInstance (mkType "Newtype" [t, mkType "_" []]) (const [])
    go Lenses = mempty
    go Prisms = mempty

memberToMethod :: InstanceMember 'PureScript -> Doc
memberToMethod InstanceMember {..} =
    hang 2 $
        hsep
            [ hsep $ textStrict <$> _memberName : _memberBindings <> ["="]
            , vsep $ textStrict <$> T.lines _memberBody
            ]

constrainWith :: Text -> PSType -> [PSType]
constrainWith name = map (mkType name . pure) . typeParams

eqConstraints :: PSType -> [PSType]
eqConstraints = constrainWith "Eq"

ordConstraints :: PSType -> [PSType]
ordConstraints = constrainWith "Ord"

showConstraints :: PSType -> [PSType]
showConstraints = constrainWith "Show"

decodeJsonConstraints :: PSType -> [PSType]
decodeJsonConstraints psType =
  (constrainWith "DecodeJson" psType) <> (constrainWith "DecodeJsonField" psType)

encodeJsonConstraints :: PSType -> [PSType]
encodeJsonConstraints = constrainWith "EncodeJson"

isEnum :: [DataConstructor lang] -> Bool
isEnum = all $ (== Nullary) . _sigValues

sumTypeToEncode :: SumType 'PureScript -> Doc
sumTypeToEncode (SumType _ cs _)
    | isEnum cs = "E.encode E.enum"
    | otherwise =
        case cs of
            [dc@(DataConstructor _ args)] ->
                hsep
                    [ "E.encode $"
                    , if isJust (nootype [dc])
                        then "unwrap"
                        else parens $ case_of [(constructorPattern dc, constructor args)]
                    , hang 2 $ ">$<" <+> nest 2 (argsToEncode args)
                    ]
            _ -> case_of (constructorToEncode <$> cs)
  where
    constructorToEncode c@(DataConstructor name args) =
        ( constructorPattern c
        , case args of
            Nullary -> "encodeJson { tag:" <+> dquotes (textStrict name) <> ", contents: jsonNull }"
            Normal as ->
                "E.encodeTagged"
                    <+> dquotes (textStrict name)
                    <+> normalExpr as
                    <+> argsToEncode args
            Record rs
                | any ((== "tag") . _recLabel) rs ->
                    "E.encodeTagged"
                        <+> dquotes (textStrict name)
                        <+> hrecord (fields rs)
                        <+> argsToEncode args
                | otherwise ->
                    hsep
                        [ "encodeJson"
                        , vrecord $
                            ("tag:" <+> dquotes (textStrict name))
                                : (recordFieldToJson <$> NE.toList rs)
                        ]
        )
    recordFieldToJson (RecordEntry name t) =
        textStrict name
            <> colon
            <+> "flip E.encode"
            <+> textStrict name
            <+> typeToEncode t
    argsToEncode Nullary = "E.null"
    argsToEncode (Normal (t :| [])) = typeToEncode t
    argsToEncode (Normal ts) =
        parens $ "E.tuple" <+> encloseHsep lparen rparen " >/\\<" (typeToEncode <$> NE.toList ts)
    argsToEncode (Record rs) =
        parens $ "E.record" <> softline <> vrecord (fieldSignatures $ fieldEncoder <$> rs)
      where
        fieldEncoder r =
            r
                & recValue %~ mkType "_" . pure
                & recLabel <>~ renderText (":" <+> typeToEncode (_recValue r))

isTypeParam :: PSType -> PSType -> Bool
isTypeParam t typ = _typeName typ `elem` map _typeName (_typeParameters t)

spaces :: Int -> Text
spaces c = T.replicate c " "

fromEntries :: (RecordEntry a -> Text) -> [RecordEntry a] -> Text
fromEntries mkElem rs = "{ " <> inners <> " }"
  where
    inners = T.intercalate ", " $ map mkElem rs

mkFnArgs :: [RecordEntry 'PureScript] -> Text
mkFnArgs [r] = r ^. recLabel
mkFnArgs rs = fromEntries (\recE -> recE ^. recLabel <> ": " <> recE ^. recLabel) rs

flattenTuple :: [PSType] -> [PSType]
flattenTuple [] = []
flattenTuple [a] = [a]
flattenTuple [a, TypeInfo "purescript-tuples" "Data.Tuple" "Tuple" ts'] = a : flattenTuple ts'
flattenTuple (h : t) = h : flattenTuple t

typeToEncode :: PSType -> Doc
typeToEncode (TypeInfo "purescript-prelude" "Prelude" "Unit" []) = "E.unit"
typeToEncode (TypeInfo "purescript-maybe" "Data.Maybe" "Maybe" [t]) =
    parens $
        "E.maybe" <+> typeToEncode t
typeToEncode (TypeInfo "purescript-either" "Data.Either" "Either" [l, r]) =
    parens $
        "E.either" <+> typeToEncode l <+> typeToEncode r
typeToEncode (TypeInfo "purescript-tuples" "Data.Tuple" "Tuple" ts) =
    parens $
        "E.tuple" <+> parens (hsep $ punctuate " >/\\<" $ typeToEncode <$> flattenTuple ts)
typeToEncode (TypeInfo "purescript-tuples" "Data.Tuple" "Tuple" ts) =
    parens $
        "E.tuple" <+> parens (hsep $ punctuate " >/\\<" $ typeToEncode <$> flattenTuple ts)
typeToEncode (TypeInfo "purescript-ordered-collections" "Data.Map" "Map" [k, v]) =
    parens $
        "E.dictionary" <+> typeToEncode k <+> typeToEncode v
typeToEncode _ = "E.value"

sumTypeToDecode :: SumType 'PureScript -> Doc
sumTypeToDecode (SumType _ cs _)
    | isEnum cs = "D.enum"
sumTypeToDecode (SumType _ [c] _) = "$" <+> constructorToDecode False c
sumTypeToDecode (SumType t cs _) =
    line
        <> hsep
            [ "$ D.sumType"
            , t ^. typeName . to textStrict . to dquotes
            , "$ Map.fromFoldable"
            , encloseVsep lbracket rbracket comma (constructorToTagged <$> cs)
            ]
  where
    constructorToTagged dc =
        hsep
            [ dc ^. sigConstructor . to textStrict . to dquotes
            , "/\\"
            , constructorToDecode True dc
            ]

constructorToDecode :: Bool -> DataConstructor 'PureScript -> Doc
constructorToDecode True (DataConstructor name Nullary) =
    "pure" <+> textStrict name
constructorToDecode False (DataConstructor name Nullary) =
    parens $ textStrict name <+> "<$" <+> "D.null"
constructorToDecode True dc@(DataConstructor _ (Normal _)) =
    "D.content" <+> constructorToDecode False dc
constructorToDecode False (DataConstructor name (Normal (a :| []))) =
    parens $ textStrict name <+> "<$>" <+> typeToDecode a
constructorToDecode False (DataConstructor name (Normal as)) =
    parens $
        "D.tuple"
            <+> "$"
            <+> textStrict name
            <+> encloseHsep "</$\\>" mempty " </*\\>" (typeToDecode <$> NE.toList as)
constructorToDecode True dc@(DataConstructor name (Record rs))
    | any ((== "tag") . _recLabel) rs =
        "D.content" <+> constructorToDecode False dc
    | otherwise =
        parens $
            textStrict name
                <+> "<$> D.object"
                <+> dquotes (textStrict name)
                <+> vrecord (fieldSignatures $ fieldDecoder <$> rs)
  where
    fieldDecoder r =
        r
            & recValue %~ mkType "_" . pure
            & recLabel <>~ renderText (":" <+> typeToDecode (_recValue r))
constructorToDecode False (DataConstructor name (Record rs)) =
    parens $
        textStrict name
            <+> "<$> D.record"
            <+> dquotes (textStrict name)
            <+> vrecord (fieldSignatures $ fieldDecoder <$> rs)
  where
    fieldDecoder r =
        r
            & recValue %~ mkType "_" . pure
            & recLabel <>~ renderText (":" <+> typeToDecode (_recValue r))

typeToDecode :: PSType -> Doc
typeToDecode (TypeInfo "purescript-prelude" "Prelude" "Unit" []) = "D.unit"
typeToDecode (TypeInfo "purescript-maybe" "Data.Maybe" "Maybe" [t]) =
    parens $
        "D.maybe" <+> typeToDecode t
typeToDecode (TypeInfo "purescript-either" "Data.Either" "Either" [l, r]) =
    parens $
        "D.either" <+> typeToDecode l <+> typeToDecode r
typeToDecode (TypeInfo "purescript-tuples" "Data.Tuple" "Tuple" ts) =
    parens $
        "D.tuple" <+> encloseHsep lparen rparen " </\\>" (typeToDecode <$> flattenTuple ts)
typeToDecode (TypeInfo "purescript-ordered-collections" "Data.Map" "Map" [k, v]) =
    parens $
        "D.dictionary" <+> typeToDecode k <+> typeToDecode v
typeToDecode _ = "D.value"

sumTypeToOptics :: SumType 'PureScript -> Doc
sumTypeToOptics st =
    vsep $ punctuate line $ constructorOptics st <> recordOptics st

constructorOptics :: SumType 'PureScript -> [Doc]
constructorOptics (SumType t cs _) = constructorToOptic (length cs > 1) t <$> cs

recordOptics :: SumType 'PureScript -> [Doc]
recordOptics st@(SumType _ [DataConstructor _ (Record rs)] _) =
    recordEntryToLens st <$> filter hasUnderscore (NE.toList rs)
recordOptics _ = mempty

hasUnderscore :: RecordEntry lang -> Bool
hasUnderscore (RecordEntry name _) = "_" `T.isPrefixOf` name

constructorToOptic
    :: Bool -> TypeInfo 'PureScript -> DataConstructor 'PureScript -> Doc
constructorToOptic hasOtherConstructors typeInfo (DataConstructor n args) =
    case (args, hasOtherConstructors) of
        (Nullary, False) -> iso pName typeInfo psUnit "(const unit)" $ parens ("const" <+> cName)
        (Nullary, True) -> prism pName typeInfo psUnit cName "unit" $ parens ("const" <+> cName)
        (Normal (t :| []), False) -> newtypeIso pName typeInfo t
        (Normal (t :| []), True) -> prism pName typeInfo t (parens $ normalPattern n [t]) "a" cName
        (Normal ts, _)
            | hasOtherConstructors -> prism pName typeInfo toType fromExpr toExpr toMorph
            | otherwise -> iso pName typeInfo toType fromMorph toMorph
          where
            fields' = fields $ typesToRecord ts
            toType = recordType $ typesToRecord ts
            fromExpr = parens $ normalPattern n ts
            toExpr = hrecord fields'
            fromMorph = parens $ lambda fromExpr toExpr
            toMorph = parens $ lambda toExpr fromExpr
        (Record rs, False) -> newtypeIso pName typeInfo $ recordType rs
        (Record rs, True) ->
            prism pName typeInfo (recordType rs) fromExpr toExpr cName
          where
            fromExpr = parens $ pattern n toExpr
            toExpr = "a"
  where
    cName = textStrict n
    pName = "_" <> textStrict n
    recordType = (`mkType` []) . renderText . hrecord . fieldSignatures

typesToRecord :: NonEmpty PSType -> NonEmpty (RecordEntry 'PureScript)
typesToRecord = fmap (uncurry RecordEntry) . NE.zip (T.singleton <$> ['a' ..])

iso :: Doc -> PSType -> PSType -> Doc -> Doc -> Doc
iso name fromType toType fromMorph toMorph =
    def
        name
        []
        []
        (mkType "Iso'" [fromType, toType])
        ("iso" <+> fromMorph <+> toMorph)

prism :: Doc -> PSType -> PSType -> Doc -> Doc -> Doc -> Doc
prism name fromType toType previewPattern previewExpr inject =
    def
        name
        []
        []
        (mkType "Prism'" [fromType, toType])
        ( "prism'"
            <+> inject
            <+> case_of
                [ (previewPattern, "Just" <+> previewExpr)
                , ("_", "Nothing")
                ]
        )

newtypeIso :: Doc -> PSType -> PSType -> Doc
newtypeIso name fromType toType =
    def
        name
        []
        []
        (mkType "Iso'" [fromType, toType])
        "_Newtype"

recordEntryToLens :: SumType 'PureScript -> RecordEntry 'PureScript -> Doc
recordEntryToLens (SumType t _ _) e =
    if hasUnderscore e
        then
            vsep
                [ signature True lensName [] [] $ mkType "Lens'" [t, e ^. recValue]
                , lensName <+> "= _Newtype <<< prop" <+> parens ("Proxy :: _" <> dquotes recName)
                ]
        else mempty
  where
    recName = e ^. recLabel . to textStrict
    lensName = e ^. recLabel . to (T.drop 1) . to textStrict

unlessM :: (Monad m) => m Bool -> m () -> m ()
unlessM mbool action = mbool >>= flip unless action

constructorPattern :: DataConstructor 'PureScript -> Doc
constructorPattern (DataConstructor name Nullary)     = nullaryPattern name
constructorPattern (DataConstructor name (Normal ts)) = normalPattern name ts
constructorPattern (DataConstructor name (Record rs)) = recordPattern name rs

constructor :: DataConstructorArgs 'PureScript -> Doc
constructor Nullary     = nullaryExpr
constructor (Normal ts) = normalExpr ts
constructor (Record rs) = hrecord $ fields rs

nullaryPattern :: Text -> Doc
nullaryPattern = textStrict

nullaryExpr :: Doc
nullaryExpr = "unit"

normalPattern :: Text -> NonEmpty PSType -> Doc
normalPattern name = pattern name . hsep . normalLabels

normalExpr :: NonEmpty PSType -> Doc
normalExpr (_ :| []) = "a"
normalExpr ts        = parens . hsep . punctuate " /\\" $ normalLabels ts

normalLabels :: NonEmpty PSType -> [Doc]
normalLabels = fmap char . zipWith const ['a' ..] . NE.toList

recordPattern :: Text -> NonEmpty (RecordEntry 'PureScript) -> Doc
recordPattern name = pattern name . hrecord . fields

vrecord :: [Doc] -> Doc
vrecord = encloseVsep lbrace rbrace comma

hrecord :: [Doc] -> Doc
hrecord = encloseHsep lbrace rbrace comma

fields :: NonEmpty (RecordEntry 'PureScript) -> [Doc]
fields = fmap field . NE.toList

field :: RecordEntry 'PureScript -> Doc
field = textStrict . _recLabel

fieldSignatures :: NonEmpty (RecordEntry 'PureScript) -> [Doc]
fieldSignatures = fmap fieldSignature . NE.toList

fieldSignature :: RecordEntry 'PureScript -> Doc
fieldSignature = uncurry signature' . (field &&& _recValue)

pattern :: Text -> Doc -> Doc
pattern name = (textStrict name <+>)

case_of :: [(Doc, Doc)] -> Doc
case_of = caseOf "_"

caseOf :: Doc -> [(Doc, Doc)] -> Doc
caseOf scrutinee [(p, b)] =
    hsep ["case", scrutinee, "of", branch p b]
caseOf scrutinee branches =
    vsep $ hsep ["case", scrutinee, "of"] : (indent 2 . uncurry branch <$> branches)

branch :: Doc -> Doc -> Doc
branch p body = hsep [p, "->", body]

lambda :: Doc -> Doc -> Doc
lambda variables body = backslash <> branch variables body

signature' :: Doc -> PSType -> Doc
signature' name = signature False name [] []

signature :: Bool -> Doc -> [PSType] -> [PSType] -> PSType -> Doc
signature topLevel name constraints params ret =
    hsep $ catMaybes [Just name, Just "::", forAll, constraintsDoc, paramsDoc, Just $ typeInfoToDecl ret]
  where
    forAll = case (topLevel, allTypes >>= typeParams) of
        (False, _) -> Nothing
        (_, []) -> Nothing
        (_, ps) -> Just $ "forall" <+> hsep (typeInfoToDoc <$> nubBy (on (==) _typeName) ps) <> "."
    allTypes = ret : constraints <> params
    constraintsDoc = case constraints of
        [] -> Nothing
        cs -> Just $ hsep ((<+> "=>") . typeInfoToDecl <$> cs)
    paramsDoc = case params of
        [] -> Nothing
        ps -> Just $ hsep ((<+> "->") . typeInfoToDecl <$> ps)

def :: Doc -> [PSType] -> [(Doc, PSType)] -> PSType -> Doc -> Doc
def name constraints params ret body =
    vsep
        [ signature True name constraints (snd <$> params) ret
        , hsep $ name : (fst <$> params) <> ["=", body]
        ]

mkType :: Text -> [PSType] -> PSType
mkType = TypeInfo "" ""

typeParams :: PSType -> [PSType]
typeParams = filter (isLower . T.head . _typeName) . flattenTypeInfo

encloseHsep :: Doc -> Doc -> Doc -> [Doc] -> Doc
encloseHsep left right sp ds =
    case ds of
        [] -> left <> right
        _  -> left <> hsep (punctuate sp ds) <> right

encloseVsep :: Doc -> Doc -> Doc -> [Doc] -> Doc
encloseVsep left right sp ds =
    case ds of
        [] -> left <> right
        [d] -> left <+> d <+> right
        _ -> nest 2 $ linebreak <> vsep (zipWith (<+>) (left : repeat (hang 2 sp)) ds <> [right])
