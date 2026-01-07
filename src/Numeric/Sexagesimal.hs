{-# LANGUAGE TypeApplications #-}

module Numeric.Sexagesimal where

import Data.List (intercalate)
import Text.ParserCombinators.ReadPrec
import Text.ParserCombinators.ReadP (char)
import Text.Printf (printf)
import Text.Read

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

instance Read Sexagesimal where
  readPrec = Sexagesimal <$> parseSexagesimal

parseSexagesimal :: ReadPrec Rational
parseSexagesimal = fmap fromSexagesimalDigits $
  (:) <$> signed <*> many (colon *> unsigned)
 where
  colon = lift $ char ':'
  signed = readPrec
  unsigned = fromEnum @Word <$> readPrec
  many p = pure [] +++ some p
  some p = (:) <$> p <*> many p

fromSexagesimalDigits :: [Int] -> Rational
fromSexagesimalDigits [] = 0
fromSexagesimalDigits (d : ds)
  | d < 0 = negate $ fromSexagesimalDigits (negate d : ds)
  | otherwise = toRational d + fromSexagesimalDigits ds / 60
