{-# LANGUAGE TypeApplications #-}

module Main where

import Numeric.Sexagesimal (Sexagesimal)

import Numeric (readFloat, readSigned)
import Data.Foldable (for_)
import System.Environment (getArgs)

main :: IO ()
main = do
  numbers <- map readNumber <$> getArgs
  for_ numbers $
    print @Sexagesimal . realToFrac

readNumber :: String -> Rational
readNumber s = case readSigned readFloat s of
  [(r, "")] -> r
  _ -> error $ "Bad number: " <> s
