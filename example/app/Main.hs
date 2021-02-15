module Main where

import qualified MyLib (main)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib.main
