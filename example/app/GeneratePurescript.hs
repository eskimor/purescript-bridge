module Main where

import           Control.Lens
import           Data.Text (pack)
import           Language.PureScript.Bridge

import qualified MyLib
import           Types

frontEndRoot :: String
frontEndRoot = "src"

-- https://discourse.purescript.org/t/latest-and-greatest-haskell-purescript-serialization/1640/6
main :: IO ()
main = do
  writePSTypesWith
    frontEndRoot
    (buildBridge myBridge)
    myTypes
