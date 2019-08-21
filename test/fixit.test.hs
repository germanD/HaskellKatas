module Fixit.Test where

import Control.Monad
import Fixit (reverse', foldr')
import Data.Function
import Test.Hspec
import Text.Printf

fixFoldr = fix foldr'
fixReverse = fix reverse'

-- Codewars wraps HSpec: http://hspec.github.io/
main = hspec $ do
  describe (show input1) $ do
    it (printf "sum of %s should return %s" (show input1) (show expected1)) 
    $ do fixFoldr (+) 0 input1 `shouldBe` expected1
  describe (show input1) $ do
    it (printf "reverse of %s is %s" (show input1) (show expected2)) 
    $ do fixReverse input1 `shouldBe` expected2 
  describe (show input4) $ do
    it (printf "reverse of %s is %s" (show input4) (show expected4)) 
    $ do fixReverse input4 `shouldBe` expected4 
  describe ("Builtins") $ do 
    it "should not be using reverse or foldr"
    $ do hidden [ FromModule "Prelude" "reverse"
                , FromModule "Prelude" "foldr" ]
  describe "FixFoldr should be Lazy" $ do
    it "should take finite sequences of infinite lists" $ do 
    take 5 (fixFoldr (:) [] [1..]) `shouldBe` [1..5]
  where
    input1 = [2,3,5,7,11]
    expected1 = 28
    expected2 = [11,7,5,3,2]
    input4 = "Reverse"
    expected4 = "esreveR"
