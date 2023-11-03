module Main where

import qualified Numeric.Sexagesimal (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  Numeric.Sexagesimal.someFunc
