module Numeric.Sexagesimal where

import Data.List (intercalate)
import Text.Printf (printf)

newtype Sexagesimal = Sexagesimal Rational
  deriving (Eq, Ord, Enum, Num, Fractional, Real, RealFrac)

instance Show Sexagesimal where
  show (Sexagesimal r) = showSexagesimal r

showSexagesimal :: Rational -> String
showSexagesimal = intercalate ":" . zipWith ($) (show : repeat showPadded) . asSexagesimalDigits
 where showPadded = printf "%02d"

asSexagesimalDigits :: Rational -> [Int]
asSexagesimalDigits 0 = [0]
asSexagesimalDigits s = go s where
  go 0 = []
  go x = i : go (abs f * 60)
    where (i, f) = properFraction x
