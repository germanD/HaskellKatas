{- Fix It! Kata by gans

https://www.codewars.com/kata/5443dd2d7fc4478154000ac6/train/haskell

The fix function can be defined as:

fix :: (a -> a) -> a
fix f = let x = f x in x

If we regard this as a language primitive, any recursive function can
be written without using recursion. Write foldr' and reverse' as
non-recursive functions that can be 'fixed' to foldr and reverse as
follows:

foldr = fix foldr'
reverse = fix reverse'

For a more detailed explanation of the fix function, see
http://en.wikipedia.org/wiki/Fixed-point_combinator


Note: foldr is lazy, so your foldr' should be lazy too. Also, your
foldr' need only work on lists - it does not have to work for an
arbitrary Foldable functor.

-}

module Fixit where
import Prelude hiding (reverse, foldr)
import Data.Function (fix)

reverse' :: ([a] -> [a]) -> [a] -> [a]
reverse' f []     = []
reverse' f (x:xs) = f xs ++ [x]

foldr' :: ( (a -> b -> b) -> b -> [a] -> b) -> (a -> b -> b) -> b -> [a] -> b
foldr' f g e []     = e
foldr' f g e (x:xs) = g x $ f g e xs 

{-
fixReverse = fix reverse'
fixFoldr = fix foldr'
-}
