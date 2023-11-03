module Main (main) where

import Numeric.Sexagesimal
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Sexagesimal" $ do
  describe "Show" $ do
    it "formats a small positive integer correctly" $
      show (42 :: Sexagesimal) `shouldBe` "42"
    it "formats a small negative integer correctly" $
      show (-42 :: Sexagesimal) `shouldBe` "-42"
    it "formats pi correctly" $
      show (realToFrac (pi :: Double) :: Sexagesimal) `shouldBe`
        "3:08:29:44:00:47:25:53:07:23:43:33:18:45:57:12:18:17:29:34:43:53:12:11:15"
    it "formats -pi correctly" $
      show (realToFrac (-pi :: Double) :: Sexagesimal) `shouldBe`
        "-3:08:29:44:00:47:25:53:07:23:43:33:18:45:57:12:18:17:29:34:43:53:12:11:15"
