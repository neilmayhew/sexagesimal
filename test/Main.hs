module Main (main) where

import Numeric.Sexagesimal
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Sexagesimal" $ do
  describe "Show" $
    it "formats a small integer correctly" $
      show (42 :: Sexagesimal) `shouldBe` "42"
