{-# LANGUAGE TypeApplications #-}

module Main where

import Numeric.Sexagesimal (Sexagesimal)

main :: IO ()
main = print $ realToFrac @Double @Sexagesimal pi
