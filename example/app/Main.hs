module Main where

import           Control.Lens
import           Data.Text (pack)
import           Language.PureScript.Bridge
import qualified MyLib (main)
import           Types

main :: IO ()
main = MyLib.main
