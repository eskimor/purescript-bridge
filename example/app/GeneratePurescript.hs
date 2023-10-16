module Main where

import           Control.Lens
import           Data.Text (pack)
import           Language.PureScript.Bridge

import qualified MyLib
import           Types

-- https://discourse.purescript.org/t/latest-and-greatest-haskell-purescript-serialization/1640/6
main :: IO ()
main = do
  writePSTypesWith "src" (buildBridge myBridge) myTypes
