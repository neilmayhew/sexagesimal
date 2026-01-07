{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Numeric.Sexagesimal
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Sexagesimal" $ do
  describe "Show" $ do
    let shouldShow = shouldBe . show @Sexagesimal
    it "formats a small positive integer correctly" $
      42 `shouldShow` "42"
    it "formats a small negative integer correctly" $
      (-42) `shouldShow` "-42"
    it "formats pi correctly" $
      realToFrac @Double pi `shouldShow`
        "3:08:29:44:00:47:25:53:07:23:43:33:18:45:57:12:18:17:29:34:43:53:12:11:15"
    it "formats -pi correctly" $
      realToFrac @Double (-pi) `shouldShow`
        "-3:08:29:44:00:47:25:53:07:23:43:33:18:45:57:12:18:17:29:34:43:53:12:11:15"

  describe "Read" $ do
    let shouldRead = shouldBe . read @Sexagesimal
    it "parses a small positive integer correctly" $
      "42" `shouldRead` 42
    it "parses a small negative integer correctly" $
      "-42" `shouldRead` (-42)
    it "parses pi correctly" $
      "3:08:29:44:00:47:25:53:07:23:43:33:18:45:57:12:18:17:29:34:43:53:12:11:15"
        `shouldRead` realToFrac @Double pi
    it "parses -pi correctly" $
      "-3:08:29:44:00:47:25:53:07:23:43:33:18:45:57:12:18:17:29:34:43:53:12:11:15"
        `shouldRead` realToFrac @Double (-pi)
