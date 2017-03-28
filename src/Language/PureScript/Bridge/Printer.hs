{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.PureScript.Bridge.Printer where

import           Control.Lens
import           Control.Monad
import           Data.Map.Strict                     (Map)
import qualified Data.Map.Strict                     as Map
import           Data.Monoid
import           Data.Set                            (Set)
import qualified Data.Set                            as Set
import           Data.Text                           (Text)
import qualified Data.Text                           as T
import qualified Data.Text.IO                        as T
import           System.Directory
import           System.FilePath


import           Language.PureScript.Bridge.SumType
import           Language.PureScript.Bridge.TypeInfo


data Module (lang :: Language) = PSModule {
  psModuleName  :: !Text
, psImportLines :: !(Map Text ImportLine)
, psTypes       :: ![SumType lang]
} deriving Show

type PSModule = Module 'PureScript

data ImportLine = ImportLine {
  importModule :: !Text
, importTypes  :: !(Set Text)
} deriving Show

type Modules = Map Text PSModule
type ImportLines = Map Text ImportLine

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
moduleToText m = T.unlines $
  "-- File auto generated by purescript-bridge! --"
  : "module " <> psModuleName m <> " where\n"
  : map importLineToText allImports
  ++ [ ""
     , "import Prelude"
     , "import Data.Generic (class Generic)"
     , ""
     ]
  ++ map sumTypeToText (psTypes m)
  where
    otherImports = importsFromList _lensImports
    allImports = Map.elems $ mergeImportLines otherImports (psImportLines m)

_lensImports :: [ImportLine]
_lensImports = [
    ImportLine "Data.Maybe" $ Set.fromList ["Maybe(..)"]
  -- , ImportLine "Prelude" mempty
  , ImportLine "Data.Lens" $ Set.fromList ["Prism'", "Lens'", "prism'", "lens"]
  ]

importLineToText :: ImportLine -> Text
importLineToText l = "import " <> importModule l <> " (" <> typeList <> ")"
  where
    typeList = T.intercalate ", " (Set.toList (importTypes l))

sumTypeToText :: SumType 'PureScript -> Text
sumTypeToText st =
  sumTypeToTypeDecls st
    <> "\n"
    <> sep
    <> "\n"
    <> sumTypeToPrismsAndLenses st
    <> sep
  where
    sep = T.replicate 80 "-"

sumTypeToTypeDecls :: SumType 'PureScript -> Text
sumTypeToTypeDecls st@(SumType t cs) = T.unlines $
    dataOrNewtype cs <> " " <> typeInfoToText True t <> " ="
  : "    " <> T.intercalate "\n  | " (map (constructorToText 4) cs)
  : [ "\nderive instance generic" <> _typeName t <> " :: " <> genericConstrains <> genericInstance t ]
  where
    genericInstance = ("Generic " <>) . typeInfoToText False
    genericConstrains
        | stpLength == 0 = mempty
        | otherwise = (<> " => ") $
            if stpLength == 1
                then genericConstrainsInner
                else bracketWrap genericConstrainsInner
    genericConstrainsInner = T.intercalate ", " $ map genericInstance sumTypeParameters
    stpLength = length sumTypeParameters
    bracketWrap x = "(" <> x <> ")"
    sumTypeParameters = filter isTypeParam . Set.toList $ getUsedTypes st
    isTypeParam typ = _typeName typ `elem` map _typeName (_typeParameters t)
    dataOrNewtype [constr]
      | either isSingletonList (const True) (_sigValues constr) = "newtype"
    dataOrNewtype _   = "data"
    isSingletonList [_] = True
    isSingletonList _  = False

sumTypeToPrismsAndLenses :: SumType 'PureScript -> Text
sumTypeToPrismsAndLenses st = sumTypeToPrisms st <> sumTypeToLenses st

sumTypeToPrisms :: SumType 'PureScript -> Text
sumTypeToPrisms st = T.unlines $ map (constructorToPrism moreThan1 st) cs
  where
    cs = st ^. sumTypeConstructors
    moreThan1 = length cs > 1


sumTypeToLenses :: SumType 'PureScript -> Text
-- Match on SumTypes with a single DataConstructor (that's a list of a single element)
sumTypeToLenses st@(SumType _ [_]) = T.unlines $ recordEntryToLens st <$> dcName <*> dcRecords
  where
    cs = st ^. sumTypeConstructors
    dcName = lensableConstructor ^.. traversed.sigConstructor
    dcRecords = lensableConstructor ^.. traversed.sigValues._Right.traverse.filtered hasUnderscore
    hasUnderscore e = e ^. recLabel.to (T.isPrefixOf "_")
    lensableConstructor = filter singleRecordCons cs ^? _head
    singleRecordCons (DataConstructor _ (Right _)) = True
    singleRecordCons _                             = False
sumTypeToLenses _ = ""

constructorToText :: Int -> DataConstructor 'PureScript -> Text
constructorToText _ (DataConstructor n (Left ts))  = n <> " " <> T.intercalate " " (map (typeInfoToText False) ts)
constructorToText indentation (DataConstructor n (Right rs)) =
       n <> " {\n"
    <> spaces (indentation + 2) <> T.intercalate intercalation (map recordEntryToText rs) <> "\n"
    <> spaces indentation <> "}"
  where
    intercalation = "\n" <> spaces indentation <> "," <> " "

spaces :: Int -> Text
spaces c = T.replicate c " "


typeNameAndForall :: SumType 'PureScript -> (Text, Text)
typeNameAndForall st = (typName, forAll)
  where
    typName = typeInfoToText False (st ^. sumTypeInfo)
    forAllParams = st ^.. sumTypeInfo.typeParameters.traversed.to (typeInfoToText False)
    forAll = case forAllParams of
      [] -> " :: "
      cs -> " :: forall " <> T.intercalate " " cs <> ". "
    -- textParameters = map (typeInfoToText False) params

fromEntries :: (RecordEntry a -> Text) -> [RecordEntry a] -> Text
fromEntries mkElem rs = "{ " <> inners <> " }"
  where
    inners = T.intercalate ", " $ map mkElem rs

mkFnArgs :: [RecordEntry 'PureScript] -> Text
mkFnArgs [r] = r ^. recLabel
mkFnArgs rs  = fromEntries (\recE -> recE ^. recLabel <> ": " <> recE ^. recLabel) rs

mkTypeSig :: [RecordEntry 'PureScript] -> Text
mkTypeSig [] = "Unit"
mkTypeSig [r] = typeInfoToText False $ r ^. recValue
mkTypeSig rs = fromEntries recordEntryToText rs

constructorToPrism :: Bool -> SumType 'PureScript -> DataConstructor 'PureScript -> Text
constructorToPrism otherConstructors st (DataConstructor n args) =
  case args of
    Left cs  -> pName <> forAll <>  "Prism' " <> typName <> " " <> mkTypeSig types <> "\n"
             <> pName <> " = prism' " <> getter <> " f\n"
             <> spaces 2 <> "where\n"
             <> spaces 4 <> "f " <> mkF cs
             <> otherConstructorFallThrough
      where
        mkF [] = n <> " = Just unit\n"
        mkF _  = "(" <> n <> " " <> T.unwords (map _recLabel types) <> ") = Just $ " <> mkFnArgs types <> "\n"
        getter | cs == [] = "(\\_ -> " <> n <> ")"
               | length cs == 1   = n
               | otherwise = "(\\{ " <> T.intercalate ", " cArgs <> " } -> " <> n <> " " <> T.intercalate " " cArgs <> ")"
          where
            cArgs = map (T.singleton . fst) $ zip ['a'..] cs
        types = [RecordEntry (T.singleton label) t | (label, t) <- zip ['a'..] cs]
    Right rs -> pName <> forAll <> "Prism' " <> typName <> " { " <> recordSig <> "}\n"
             <> pName <> " = prism' " <> n <> " f\n"
             <> spaces 2 <> "where\n"
             <> spaces 4 <> "f (" <> n <> " r) = Just r\n"
             <> otherConstructorFallThrough
      where
        recordSig = T.intercalate ", " (map recordEntryToText rs)
  where
    (typName, forAll) = typeNameAndForall st
    pName = "_" <> n
    otherConstructorFallThrough | otherConstructors = spaces 4 <> "f _ = Nothing\n"
                                | otherwise = "\n"

recordEntryToLens :: SumType 'PureScript -> Text -> RecordEntry 'PureScript -> Text
recordEntryToLens st constructorName e =
  case hasUnderscore of
    False -> ""
    True ->
         lensName <> forAll <>  "Lens' " <> typName <> " " <> recType <> "\n"
      <> lensName <> " = lens get set\n  where\n"
      <> spaces 4 <> "get (" <> constructorName <> " r) = r." <> recName <> "\n"
      <> spaces 4 <> "set (" <> constructorName <> " r) = " <> setter
  where
    (typName, forAll) = typeNameAndForall st
    setter = constructorName <>  " <<< r { " <> recName <> " = _ }\n"
    recName = e ^. recLabel
    lensName = T.drop 1 recName
    recType = typeInfoToText False (e ^. recValue)
    hasUnderscore = e ^. recLabel.to (T.isPrefixOf "_")

recordEntryToText :: RecordEntry 'PureScript -> Text
recordEntryToText e = _recLabel e <> " :: " <> typeInfoToText True (e ^. recValue)


typeInfoToText :: Bool -> PSType -> Text
typeInfoToText topLevel t = if needParens then "(" <> inner <> ")" else inner
  where
    inner = _typeName t <>
      if pLength > 0
        then " " <> T.intercalate " " textParameters
        else ""
    params = _typeParameters t
    pLength = length params
    needParens = not topLevel && pLength > 0
    textParameters = map (typeInfoToText False) params

sumTypesToModules :: Modules -> [SumType 'PureScript] -> Modules
sumTypesToModules = foldr sumTypeToModule

sumTypeToModule :: SumType 'PureScript -> Modules -> Modules
sumTypeToModule st@(SumType t _) = Map.alter (Just . updateModule) (_typeModule t)
  where
    updateModule Nothing = PSModule {
          psModuleName = _typeModule t
        , psImportLines = dropSelf $ typesToImportLines Map.empty (getUsedTypes st)
        , psTypes = [st]
        }
    updateModule (Just m) = m {
        psImportLines = dropSelf $ typesToImportLines (psImportLines m) (getUsedTypes st)
      , psTypes = st : psTypes m
      }
    dropSelf = Map.delete (_typeModule t)

typesToImportLines :: ImportLines -> Set PSType -> ImportLines
typesToImportLines = foldr typeToImportLines

typeToImportLines :: PSType -> ImportLines -> ImportLines
typeToImportLines t ls = typesToImportLines (update ls) (Set.fromList (_typeParameters t))
  where
    update = if not (T.null (_typeModule t))
                then Map.alter (Just . updateLine) (_typeModule t)
                else id

    updateLine Nothing = ImportLine (_typeModule t) (Set.singleton (_typeName t))
    updateLine (Just (ImportLine m types)) = ImportLine m $ Set.insert (_typeName t) types

importsFromList :: [ImportLine] -> Map Text ImportLine
importsFromList ls = let
    pairs = zip (map importModule ls) ls
    merge a b = ImportLine (importModule a) (importTypes a `Set.union` importTypes b)
  in
    Map.fromListWith merge pairs

mergeImportLines :: ImportLines -> ImportLines -> ImportLines
mergeImportLines = Map.unionWith mergeLines
  where
    mergeLines a b = ImportLine (importModule a) (importTypes a `Set.union` importTypes b)

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mbool action = mbool >>= flip unless action
