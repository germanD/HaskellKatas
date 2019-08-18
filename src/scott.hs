{- Scott Encoding Katta by xenoexplorator -}
{- https://www.codewars.com/kata/scott-encoding/train/haskell -}

{- Original Description

Can you make algebraic data types even more functional? Of course!

Scott encoding represents data as funtion that apply their argument
(also a function) to their value. This approach is similar to using
pattern matching on regular ADTs to extract and use their content.

You are given types representing the Scott encoding of four common
Haskell types: Maybe, lists, Either and pairs (2-tuples).

Your task is to implement conversion functions between these regular
types and their Scott encoding.

In addition, you will have to implement the following common
operations using the provided Scott-encoded data types:

fst and snd, functions to extract the content of a pair

swap, a function that exchanges the content of a pair

curry, a function to turn functions of pairs into functions of two
arguments

uncurry, a function to turn functions of two arguments into functions
of pairs

isJust and isNothing, predicates testing wether a Maybe contains data

isLeft and isRight, predicates testing which side is contained in an
Either

cons, a function to prepend an element to a list

concat, a function to contanetate two lists

catMaybes, a function to flatten a list of Maybes by removing Nothings

null, a predicate testing wether a list is empty

length, a function returning the number of elements in a list

map, a function to transform the contents of a list according to a
given function

zip, a funtion to merge two lists into a list of pairs

partition, a function that splits a list of Eithers in a pair of Lefts
and Rights

foldl and foldr, functions to reduce a list to a single value by
successive application of a given function

take, a function to limit a list to a number of initial elements

-}

{-# LANGUAGE ScopedTypeVariables, Rank2Types #-}

module ScottEncoding where

import Prelude hiding (null, length, map, foldl, foldr, take,
  fst, snd, curry, uncurry, concat, zip, (++), tail)

newtype SPair a b = SPair { runPair :: forall c. (a -> b -> c) -> c }

toPair :: SPair a b -> (a,b)
toPair m = runPair m (,)

fromPair :: (a,b) -> SPair a b
fromPair (x,y) = SPair $ \ f -> f x y

-- uncurry (a.k.a fold/split for SPairs) is a natural transformation from
-- (,)-algebras to SPair-algebras

uncurry :: (a -> b -> c) -> SPair a b -> c
uncurry f p = runPair p f

fst :: SPair a b -> a
fst  = uncurry (\ x y -> x)

snd :: SPair a b -> b
snd  = uncurry (\ x y -> y)

-- curry transforms SPair algebras into (,) algebras

curry :: (SPair a b -> c) -> (a -> b -> c)
curry f x y = f $ fromPair (x,y)

pmap :: (a -> c) -> (b -> d) -> SPair a b -> SPair c d
pmap  f g = uncurry $ \ a b -> fromPair (f a, g b)

swap :: SPair a b -> SPair b a
swap  = uncurry $ \ a b -> fromPair (b, a)

pair :: a -> b -> SPair a b
pair a b = fromPair (a,b)

newtype SMaybe a = SMaybe { runMaybe :: forall b. b -> (a -> b) -> b }

caseM :: b -> (a -> b) -> SMaybe a -> b
caseM e f m = runMaybe m e f

toMaybe :: SMaybe a -> Maybe a
toMaybe =  caseM Nothing Just

snothing :: SMaybe a
snothing = SMaybe $ const

sjust  :: a -> SMaybe a
sjust  = \ x -> SMaybe $ \ _ f -> f x

fromMaybe :: Maybe a -> SMaybe a
fromMaybe Nothing = snothing
fromMaybe (Just n) = sjust n

isJust :: SMaybe a -> Bool
isJust =  caseM False (const True)

isNothing :: SMaybe a -> Bool
isNothing = caseM True (const False)


newtype SEither a b = SEither { runEither :: forall c. (a -> c) -> (b -> c) -> c }

toEither :: SEither a b -> Either a b
toEither e = runEither e Left Right

fromEither :: Either a b -> SEither a b
fromEither (Left a)  = SEither $ \ f _ -> f a 
fromEither (Right b) = SEither $ \ _ g -> g b 

isLeft :: SEither a b -> Bool
isLeft e = runEither e (const True) (const False)

isRight :: SEither a b -> Bool
isRight e = runEither e (const False) (const True) 

caseE :: (a -> c) -> (b -> c) -> SEither a b -> c
caseE f g e = runEither e f g

newtype SList a = SList { runList :: forall b. b -> (a -> SList a -> b) -> b }

toList :: SList a -> [a]
toList xs = runList xs [] (\ x ms -> x : toList ms)

fromList :: [a] -> SList a
fromList [] = SList $ const
fromList (x:xs) = SList $ \ _ f -> f x $ fromList xs

cons :: a -> SList a -> SList a
cons x ls = SList $ \ _ f -> f x ls

snil :: SList a
snil = SList $ \ e _ -> e

null :: SList a -> Bool
null xs = runList xs True $ \ _ _ -> False

caseL :: c -> (a -> SList a -> c) -> SList a -> c
caseL e g xs = runList xs e g 

foldr :: (a -> b -> b) -> b -> SList a -> b
foldr f e l = runList l e $ \ x m -> f x $ foldr f e m

foldl :: (b -> a -> b) -> b -> SList a -> b
foldl f e l = runList l e $ \ x  -> foldl f $ f e x

map :: (a -> b) -> SList a -> SList b
map f = foldr ( \ x ms -> cons (f x) ms) snil

fhead :: SList a -> a
fhead = caseL undefined (\ x xs -> x)

tail :: SList a -> SList a
tail = caseL snil (\ x xs -> xs)

-- List Surgery

length :: SList a -> Int
length = foldr (const (+1)) 0

concat :: SList a -> SList a -> SList a
concat = foldr (\ x g -> (cons x) . g) id

rcons :: SList a -> a -> SList a
rcons xs = concat xs . (flip cons $ snil)  

take :: Int -> SList a -> SList a
take n xs = foldr falg (const snil) xs n
 where falg x rs n 
         | n <= 0    = snil
         | otherwise = cons x $ rs $ n -1 

-- TODO: Scott encoding for Nats 

catMaybes :: SList (SMaybe a) -> SList a
catMaybes = foldr falg snil
 where falg x ls = concat (caseM snil (flip cons $ snil) x) ls

zip :: SList a -> SList b -> SList (SPair a b)
zip aa bb
  | null aa || null bb = snil
  | otherwise  =
      let (a,as) = (fhead aa, tail aa)
          (b,bs) = (fhead bb, tail bb)
      in  cons (pair a b) $ zip as bs


partition :: SList (SEither a b) -> SPair (SList a) (SList b)
partition = foldr falg $ fromPair (snil :: SList a, snil :: SList b)
  where falg ab ms = let ls = fst ms
                         rs = snd ms
                     in caseE (\ x -> fromPair (cons x ls,rs))
                              (\ x -> fromPair (ls, cons x rs)) ab

