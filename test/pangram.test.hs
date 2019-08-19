module Pangram.Test where
import Pangram (isPangram)
import Test.Hspec

main = hspec $ do
  describe "isPangram" $ do
    it "should work for some examples" $ do      
      isPangram "The quick brown fox jumps over the lazy dog."  `shouldBe` True      
      isPangram "Pack my box with five dozen liquor jugs."      `shouldBe` True
      isPangram "Not every sentence is a a pangram. An example" `shouldBe` False
      isPangram "I'm not a ****nG%WAS$%$@@$@CDS pangram" `shouldBe` False
