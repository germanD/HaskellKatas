{- Isomorphisms by lolisa -}
{- https://www.codewars.com/kata/isomorphism =-}

{-

We will walk through the definition of isomorphism and define some
common isomorphisms. It is closely related to bijection (see
https://en.wikipedia.org/wiki/Bijection).

This kata possibly unlocks: Algebraic Isomorphism, Peano And Church.

Note: this kata is significantly easier in Haskell and PureScript than
in any other language. We won't reply at complaints for other
languages unless you have completed it in Haskell and/or PureScript.

P.S. The tests are CORRECT. There are some easy-to-make mistakes (see
the discussion area). Please figure out how to solve this yourself.

Detailed instructions are written in the comments of the initial solution.

If you are stuck then you can try to look at Bijection for intuition.

-}

module ISO where

import Data.Void

-- A type of `Void` have no value.
-- So it is impossible to construct `Void`,
-- unless using undefined, error, unsafeCoerce, infinite recursion, etc
-- And there is a function
-- absurd :: Void -> a
-- That get any value out of `Void`
-- We can do this becuase we can never have void in the zeroth place.

-- so, when are two type, `a` and `b`, considered equal?
-- a definition might be, it is possible to go from `a` to `b`,
-- and from `b` to `a`.
-- Going a roundway trip should leave you the same value.
-- Unfortunately it is virtually impossible to test this in Haskell.
-- This is called Isomorphism.

type ISO a b = (a -> b, b -> a)

-- given ISO a b, we can go from a to b
substL :: ISO a b -> (a -> b)
substL = fst

-- and vice versa
substR :: ISO a b -> (b -> a)
substR = snd

-- There can be more than one ISO a b
isoBool :: ISO Bool Bool
isoBool = (id, id)

isoBoolNot :: ISO Bool Bool
isoBoolNot = (not, not)

-- isomorphism is reflexive
refl :: ISO a a
refl = (id, id)

-- isomorphism is symmetric
symm :: ISO a b -> ISO b a
symm (ab,ba) =(ba,ab)

-- isomorphism is transitive
trans :: ISO a b -> ISO b c -> ISO a c
trans f g = (substL g . substL f, substR f . substR g) 

-- We can combine isomorphism:
isoTuple :: ISO a b -> ISO c d -> ISO (a, c) (b, d)
isoTuple (ab, ba) (cd, dc) = 
  (\(a, c) -> (ab a, cd c), \(b,d) -> (ba b,dc d))

isoList :: ISO a b -> ISO [a] [b]
isoList (f,g) = (map f, map g)

isoMaybe :: ISO a b -> ISO (Maybe a) (Maybe b)
isoMaybe (f,g) = (fmap f, fmap g)
                                    
-- Either is a covariant bifunctor
bimapEither :: (a -> b) -> (c -> d) -> Either a c -> Either b d
bimapEither f g (Left x)  = Left $ f x
bimapEither f g (Right x) = Right $ g x

isoEither :: ISO a b -> ISO c d -> ISO (Either a c) (Either b d)
isoEither f g =
  (bimapEither (substL f) (substL g), bimapEither (substR f) (substR g))

isoFunc :: ISO a b -> ISO c d -> ISO (a -> c) (b -> d)
isoFunc f g = 
  ( \ac -> substL g . ac . substR f, \bd -> substR g . bd . substL f)

-- Going another way is hard (and is generally impossible)
isoUnMaybe :: ISO (Maybe a) (Maybe b) -> ISO a b
-- Remember, for all valid ISO, converting and converting back
-- Is the same as the original value.
-- You need this to prove some case are impossible.
isoUnMaybe f = (fromMBB, fromMBA)
  where fromMBB a = case (substL f (Just a)) of
                      Just b -> b
                      Nothing -> substL (isoUnMaybe f) a
        fromMBA b = case (substR f (Just b)) of
                      Just a -> a
                      Nothing -> substR (isoUnMaybe f) b

-- isoUnMaybe f =
--   (fromJust . substL f . Just , fromJust . substR f . Just)
--   where fromJust (Just x) = x

-- We cannot have
-- isoUnEither :: ISO (Either a b) (Either c d) -> ISO a c -> ISO b d.
-- Note that we have
isoEU :: ISO (Either [()] ()) (Either [()] Void)
isoEU =  (leanleft, bimapEither id $ const ())
 where leanleft (Left xs)  = Left xs
       leanleft (Right ()) = Left []
-- where (), the empty tuple, has 1 value, and Void has 0 value
-- If we have isoUnEither,
-- We have ISO () Void by calling isoUnEither isoEU
-- That is impossible, since we can get a Void by substL on ISO () Void
-- So it is impossible to have isoUnEither

-- And we have isomorphism on isomorphism!
isoSymm :: ISO (ISO a b) (ISO b a)
isoSymm = (symm , symm)

lrl (ab, ba) = ba . ab

{- Testing advanced cases -}

isoMB_idL  = substL . isoUnMaybe . isoMaybe 
isoMB_idR  = substR . isoUnMaybe . isoMaybe 

isoUnMB_idL  = substL . isoMaybe . isoUnMaybe 
isoUnMB_idR  = substR . isoMaybe . isoUnMaybe  

