module Numeric.Sexagesimal where

import Control.Monad (guard)
import Data.List (intercalate)
import Text.ParserCombinators.ReadP (char)
import Text.Printf (printf)
import Text.Read (Read (readPrec), ReadPrec, lift, (+++))

-- | A newtype wrapper to read and show in sexagesimal notation
newtype Sexagesimal = Sexagesimal Rational
  deriving (Eq, Ord, Enum, Num, Fractional, Real, RealFrac)

instance Show Sexagesimal where
  show (Sexagesimal r) = showSexagesimal r

instance Read Sexagesimal where
  readPrec = Sexagesimal <$> readSexagesimal

-- Note that the output will be infinite if the denominator has a prime factor > 5
showSexagesimal :: Rational -> String
showSexagesimal = intercalate ":" . zipWith ($) (show : repeat showPadded) . toSexagesimalDigits
 where
  showPadded = printf "%02d"

-- Note that the output will be infinite if the denominator has a prime factor > 5
toSexagesimalDigits :: Rational -> [Int]
toSexagesimalDigits 0 = [0]
toSexagesimalDigits s = go s
 where
  go 0 = []
  go x = i : go (abs f * 60)
   where
    (i, f) = properFraction x

readSexagesimal :: ReadPrec Rational
readSexagesimal =
  fmap fromSexagesimalDigits $
    (:) <$> signed <*> many (colon *> unsigned)
 where
  colon = lift $ char ':'
  signed = readPrec
  unsigned = do
    i <- signed
    guard $ i >= 0
    pure i
  -- These are missing from "Text.ParserCombinators.ReadPrec"
  many p = pure [] +++ some p
  some p = (:) <$> p <*> many p

fromSexagesimalDigits :: [Int] -> Rational
fromSexagesimalDigits [] = 0
fromSexagesimalDigits (d : ds)
  | d < 0 = negate $ fromSexagesimalDigits (negate d : ds)
  | otherwise = toRational d + fromSexagesimalDigits ds / 60
