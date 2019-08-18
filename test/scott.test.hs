module ScottEncoding.Test where

import qualified Prelude (zip)	
import Prelude hiding (null, length, map, foldl, foldr, take, fst,  
                        snd, curry, uncurry, zip, concat, (++), filter,
                        nothing, just, snil) 
import ScottEncoding
import Test.Hspec

reduce :: Num a => a -> SList a -> a
reduce i sl = i + 10 * runList sl 0 reduce

testMB =   
  describe "The Maybe type" $ do
    it "can be cast to Prelude.Maybe" $ do
      toMaybe (SMaybe const) `shouldBe` (Nothing :: Maybe Int)
      toMaybe (SMaybe $ \_ f -> f 4) `shouldBe` Just 4
    it "can be cast from Prelude.Maybe" $ do
      runMaybe (fromMaybe (Nothing)) 0 (+1) `shouldBe` 0
      runMaybe (fromMaybe (Just 4)) 0 (+1) `shouldBe` 5
    it "isJust/isNothing" $ do
      isJust (fromMaybe (Nothing :: Maybe Int)) `shouldBe` False
      isJust (fromMaybe (Just 4)) `shouldBe` True
      
testP = 
  describe "The pair type" $ do
    it "can be cast to (,)" $ do
      toPair (SPair $ \f -> f 2 "hi") `shouldBe` (2, "hi")
    it "can be cast from (,)" $ do
      runPair (fromPair (2, "hi")) replicate `shouldBe` ["hi", "hi"]      
    it "swap (a,b) = (b,a) " $ do
      snd (swap (fromPair (3, 2))) `shouldBe` 3
      fst (swap (fromPair (3, 2))) `shouldBe` 2
    it " curry / uncurry  " $ do
      uncurry (+) (fromPair (3, 2)) `shouldBe` 5
      curry ( \ p -> fst p + snd p) 3 2 `shouldBe` 5

testE =
    describe "The Either type" $ do
      it "can be cast to Prelude.Either" $ do
        toEither (SEither $ \f _ -> f 3) `shouldBe` (Left 3 :: Either Int String)
        toEither (SEither $ \_ f -> f "hello") `shouldBe` (Right "hello" :: Either Int String)
      it "can be cast from Prelude.Either" $ do
        runEither (fromEither (Left 3)) show id `shouldBe` "3"
        runEither (fromEither (Right "hello" :: Either Int String)) show id `shouldBe` "hello"
      it "Left/Right" $ do
        isLeft (fromEither (Left 3))  `shouldBe` True
        isLeft (fromEither (Right 3))  `shouldBe` False
        isRight (fromEither (Left 3))  `shouldBe` False
        isRight (fromEither (Right 3))  `shouldBe` True

testL =
    describe "The list type" $ do
      it "can be cast to []" $ do
          toList (SList const) `shouldBe` ([] :: [Int])
          toList (SList $ \_ f -> f 1 (SList $ \_ g -> g 2 (SList const))) `shouldBe` [1,2]
      it "can be cast from []" $ do
          runList (fromList []) 0 reduce `shouldBe` 0
          runList (fromList [1,2]) 0 reduce `shouldBe` 21
      it "null xs?" $ do
          null snil `shouldBe` True
          null (fromList []) `shouldBe` True
          null (fromList [1,2]) `shouldBe` False
      it "cons Tests " $ do
          toList (cons 1 (fromList [])) `shouldBe` [1]
          null (cons 7 (fromList [2..5])) `shouldBe` False

testF = 
    describe "Fold/Map Properties for List" $ do
      it "foldr cons snil = id" $ do
        (toList $ foldr cons snil $ fromList [1..5]) `shouldBe` [1..5]
      it "foldl (flip cons) snil = rev" $ do
        (toList $ foldl (flip cons) snil $ fromList [1..5]) `shouldBe` (reverse [1..5])
      it "foldl rcons snil = id" $ do
        (toList $ foldl rcons snil $ fromList [1..5]) `shouldBe` [1..5]
      it "Map Functor: map id = id" $ do
        (toList $ map id $ fromList [1..5]) `shouldBe` [1..5]
      it "Map Functor: map assoc" $ do
        (toList $ map (+1) $ map (+3) $ fromList [1..5]) `shouldBe` 
          (toList $ map (+4) $ fromList [1..5])    
testD = 
     describe "Distributive Laws for Lists" $ do
      it "Concat Map : map f (concat l r) = concat (map f l) (map f r)" $ do
          (toList $ map (+1) $ concat (fromList [1..5]) (fromList [6..10])) `shouldBe` 
            (toList $ concat (map (+1) $ fromList [1..5]) (map (+1) $ fromList [6..10]))
testS = 
     describe "List Surgery" $ do
      it "take Basics" $ do
          (toList $ take 8 snil) `shouldBe` ([] :: [Int])
          (toList $ take 0 $ fromList [1..5]) `shouldBe` []
          (toList $ take 5 $ fromList [1..8]) `shouldBe` [1..5]
      it "take lenght = id" $ do
           let ms = fromList [1..5]
           (toList $ take (length ms) ms) `shouldBe` [1..5]
      it "concat Surgery" $ do
           let ls = fromList [1..5]
               rs = fromList [6..10]
            in (toList $ concat ls rs) `shouldBe` [1..10]           
      it "catMaybe . map Just = id" $ do
           let ms = fromList [1..5]
           (toList $ catMaybes $ map sjust ms) `shouldBe` [1..5]
      it "catMaybe Surgery" $ do
           let ls = map sjust $ fromList [1..5]
               rs = map sjust $ fromList [6..10]
               xs = concat ls (cons snothing rs)
           in (toList $ catMaybes xs) `shouldBe` [1..10]
            
testZ = 
       describe "List Zipping" $ do
         it "zip snils is snil" $ do
           (toList $ map toPair $ zip (snil :: SList Int) (snil :: SList Int)) `shouldBe` ([] :: [(Int,Int)]) 
         it "zip snil with sth is snil" $ do
             let rs = fromList [ 1 .. 8]
             (toList $ map toPair $ zip (snil :: SList Int) rs) `shouldBe` ([] :: [(Int,Int)]) 
         it "zip sth with snil is snil" $ do
             let ls = fromList [ 1 .. 8]
             (toList $ map toPair $ zip ls (snil :: SList Int)) `shouldBe` ([] :: [(Int,Int)]) 
         it "zip does some zipping" $ do
           let as = [1..4] :: [Int]
               bs = [5..8] :: [Int]
               ls = fromList as
               rs = fromList bs
           in (toList $ map toPair $ zip ls rs) `shouldBe` [(1,5),(2,6),(3,7),(4,8)]
main = 
  do hspec testP
     hspec testMB
     hspec testE
     hspec testL
     hspec testF
     hspec testD
     hspec testS
     hspec testZ
 
