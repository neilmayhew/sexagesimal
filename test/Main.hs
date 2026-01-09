{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Test.Hspec

import Numeric.Sexagesimal

main :: IO ()
main = hspec spec

-- Note: This is the sexagesimal representation of the closest `Double` to π.
-- The real value, to the same number of sexagesimal places, is shown below it.
-- However, they both produce the same value when converted to `Double`.
piSexagesimal :: String
piSexagesimal =
  "3:08:29:44:00:47:25:53:07:23:43:33:18:45:57:12:18:17:29:34:43:53:12:11:15"
-- 3:08:29:44:00:47:25:53:07:24:57:36:17:43:04:29:07:10:03:41:17:52:36:12:15

piFromDouble :: Sexagesimal
piFromDouble = realToFrac $ pi @Double

spec :: Spec
spec = describe "Sexagesimal" $ do
  describe "Show" $ do
    let shouldShow = shouldBe . show @Sexagesimal
    it "formats a small positive integer correctly" $
      42 `shouldShow` "42"
    it "formats a small negative integer correctly" $
      (-42) `shouldShow` "-42"
    it "formats pi correctly" $
      piFromDouble `shouldShow` piSexagesimal
    it "formats -pi correctly" $
      (-piFromDouble) `shouldShow` ('-':piSexagesimal)

  describe "Read" $ do
    let shouldRead = shouldBe . read @Sexagesimal
    it "parses a small positive integer correctly" $
      "42" `shouldRead` 42
    it "parses a small negative integer correctly" $
      "-42" `shouldRead` (-42)
    it "parses pi correctly" $
      piSexagesimal `shouldRead` piFromDouble
    it "parses -pi correctly" $
      ('-':piSexagesimal) `shouldRead` (-piFromDouble)
