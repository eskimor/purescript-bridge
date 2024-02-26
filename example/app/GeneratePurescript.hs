module Main where

import           Control.Lens
import           Data.Text (pack)
import           Language.PureScript.Bridge

import           ArgonautTypes
import           JsonHelpersTypes
import qualified MyLib
import           Types (myBridge)

-- https://discourse.purescript.org/t/latest-and-greatest-haskell-purescript-serialization/1640/6
main :: IO ()
main = do
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
