module ISOSpec where

import ISO

import Test.Hspec
import Test.QuickCheck
import Data.Either

bISO :: ISO Bool Bool
bISO = (not, not)

lrl :: ISO a b -> (a -> a)
lrl (ab, ba) = ba . ab

isoMB_idL  = substL . isoUnMaybe . isoMaybe 
isoMB_idR  = substR . isoUnMaybe . isoMaybe 

isoUnMB_idL  = substL . isoMaybe . isoUnMaybe 
isoUnMB_idR  = substR . isoMaybe . isoUnMaybe  

spec = do
  describe "subst" $ do
    it "substL" $ do
      substL bISO    True  `shouldBe` False
      substL bISO    False `shouldBe` True
      substL isoBool False `shouldBe` False
      substL isoBool True  `shouldBe` True
    it "substR" $ do
      substR bISO    True  `shouldBe` False
      substR bISO    False `shouldBe` True
      substR isoBool True  `shouldBe` True
      substR isoBool False `shouldBe` False
    it "isoEU" $ do
      isLeft (substL isoEU (Right ())) `shouldBe` True
    it "lrl isoEU (Left (replicate n ())) == Left (replicate n ())" $
      property $ \(NonNegative n) -> 
        lrl isoEU (Left (replicate n ())) == Left (replicate n ())
  describe "isoUnMaybe . isoMaybe = id" $ do
    it "L ---> L " $ do isoMB_idL refl 4 `shouldBe` 4
    it "R <--- R " $ do isoMB_idR isoBool True `shouldBe` True
  describe "isoMaybe . isoUnMaybe  = idM" $ do
    it "L ---> L " $ do 
      (isoUnMB_idL (isoMaybe isoBool) $ Just True) `shouldBe` Just True
    it "R <--- R " $ do 
      (isoUnMB_idR (isoMaybe refl) (Nothing :: Maybe ())) `shouldBe` Nothing
