module Main where

import Test.Hspec
import Model

main :: IO ()
main = hspec $ do

  describe "computeTotal" $ do

    -- Replace this test with a relevant ones
    it "should give the dummy value of 1000" $ do
      let order = Order [3.14] [42] "WHATEVER" "WHATEVER"
       in computeTotal order `shouldBe` Quantity 1000.0
