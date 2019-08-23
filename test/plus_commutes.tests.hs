module Kata.AdditionCommutes.Examples where

import Kata.AdditionCommutes
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "My own tests" $ do
    it "My first test" $ do
      "Write your own tests here!" `shouldNotBe` "Good luck!"
