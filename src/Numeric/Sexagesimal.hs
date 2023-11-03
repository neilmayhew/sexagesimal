module Numeric.Sexagesimal where

newtype Sexagesimal = Sexagesimal Rational
  deriving (Eq, Ord, Enum, Num, Fractional, Real, RealFrac)

instance Show Sexagesimal where
  show (Sexagesimal r) = show (round r :: Integer)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
