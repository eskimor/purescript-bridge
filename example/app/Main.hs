module Main where

import qualified MyLib (main)
import           Types
import           Control.Lens
import           Data.Text (pack)
import           Language.PureScript.Bridge

main :: IO ()
main = do
  writePSTypesWith "src" (buildBridge myBridge) myTypes
  MyLib.main
