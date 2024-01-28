module Main where

import           Control.Lens
import           Data.Text (pack)
import           Language.PureScript.Bridge
import qualified MyLib (main)
import           Types
import           ArgonautTypes
import           JsonHelpersTypes

main :: IO ()
main = do
  -- generate PureScript before server starts
  writePSTypesWithNamespace
    (Just . PackageName $ pack "Argonaut")
    "src"
    (buildBridge myBridge)
    myArgonautTypes
  writePSTypesWithNamespace
    (Just . PackageName $ pack "JsonHelpers")
    "src"
    (buildBridge myBridge)
    myJsonHelpersTypes

  MyLib.main
