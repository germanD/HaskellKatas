{- A+B=B+A? Prove it! Prove it! by AJFarmar -}
{- https://www.codewars.com/kata/a-plus-b-equals-b-plus-a-prove-it/train/haskell =-}

{-

Before you begin! This kata is intended as a successor to the kata
Even + Odd = Odd? Prove it! If you haven't completed it, you will find
this kata very difficult.

What's this kata about?

In this kata, you will prove the commutativity of addition, that is to
say, the fact that a + b = b + a. Specifically you will be proving it
for the natural numbers.

What can I use?

You have three useful datastructures defined for you in the module
Kata.AdditionCommutes.Definitions. You are given no more help in
creating this proof.

The natural numbers

This is a very simple definition of the natural numbers, using types.

data Z
data S n

The 'Natural' type

This can be thought of as a predicate, meaning that some number n is a
natural number. This is useful for passing numbers to our proofs.

data Natural :: * -> * where
  NumZ :: Natural Z
  NumS :: Natural n -> Natural (S n)

The 'Equal' type

This is a statement of equality on natural numbers.

data Equal :: * -> * -> * where
    EqlZ :: Equal Z Z
    EqlS :: Equal n m -> Equal (S n) (S m)
The addition type family
This is Peano's description of addition.

type family (:+:) (n :: *) (m :: *) :: *
type instance Z :+: m = m
type instance S n :+: m = S (n :+: m)

What's the final goal?  You must write a proof, ie: a function, as so:

plusCommutes :: Natural a -> Natural b -> Equal (a :+: b) (b :+: a)

This will be tested for cheats, ie: using undefined instead of the actual proof.

What problems might I have?

The challenge of this kata is getting the proof to typecheck. As such,
most of the errors produced will be type errors. Cheating or trying to
change the names of the types will cause issues. Using undefined will
very likely cause a problem, too.

Additionally, performance is a serious concern. The tests will take
only a few milliseconds to run, but the compilation will take quite a
bit of time. Please pay attention to the time it takes to run your
tests.

Good luck!

-}

{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Kata.AdditionCommutes
  ( plusCommutes ) where

-- import Kata.AdditionCommutes.Definitions
--   ( Z, S
--   , Natural(..), Equal(..)
--   , (:+:))

-- For reference, here are the definitions, if you
-- want to copy them into an IDE:

-- | The natural numbers, encoded in types.
data Z
data S n

-- | Predicate describing natural numbers.
-- | This allows us to reason with `Nat`s.
data Natural :: * -> * where
    NumZ :: Natural Z
    NumS :: Natural n -> Natural (S n)

-- | Predicate describing equality of natural numbers.
data Equal :: * -> * -> * where
    EqlZ :: Equal Z Z
    EqlS :: Equal n m -> Equal (S n) (S m)

-- | Peano definition of addition.
type family (:+:) (n :: *) (m :: *) :: *
type instance Z :+: m = m
type instance S n :+: m = S (n :+: m)

-- These are some lemmas that may be helpful.
-- They will *not* be tested, so rename them
-- if you so desire. Good luck!

-- | For any n, n = n.
reflexive :: Natural n -> Equal n n
reflexive  NumZ    = EqlZ
reflexive (NumS s) = EqlS $ reflexive s

-- | if a = b, then b = a.
symmetric :: Equal a b -> Equal b a
symmetric EqlZ = EqlZ
symmetric (EqlS pf) = EqlS $ symmetric pf

-- | if a = b and b = c, then a = c.
transitive :: Equal a b -> Equal b c -> Equal a c
transitive EqlZ pf = pf
transitive (EqlS pfL) (EqlS pfR) = EqlS $ transitive pfL pfR

-- This is the proof that the kata requires.
-- | a + b = b + a
plusCommutes :: Natural a -> Natural b -> Equal (a :+: b) (b :+: a)
plusCommutes NumZ     m = zeroPlusComm $ reflexive m
 where zeroPlusComm :: Equal a b -> Equal (Z :+: a) (b :+: Z)
       -- zeroPlusComm states that if a = b then O + a = b + 0
       zeroPlusComm EqlZ      = EqlZ
       zeroPlusComm (EqlS pf) = EqlS $ zeroPlusComm pf
plusCommutes (NumS n) m =
  let hyp1 = sucCommPlusOne $ plusCommutes n m
      hyp2 = addPOneCommAddSuc m n
  in transitive hyp1 hyp2
  where sucCommPlusOne :: Equal a b -> Equal (S a) (b :+: S Z)
        -- sucCommPlusOne says that if a = b then S a = b + 1 
        sucCommPlusOne EqlZ      = EqlS EqlZ
        sucCommPlusOne (EqlS pf) = EqlS $ sucCommPlusOne pf
        addPOneCommAddSuc :: Natural b -> Natural a -> Equal ((b :+: a) :+: S Z) (b :+: S a)
        -- addPOneCommAddSuc extends succPlusOne to say that:
        -- forall b a, (b + a) + 1 = b + S a
        addPOneCommAddSuc  NumZ    n  = symmetric $ sucCommPlusOne $ reflexive n
        addPOneCommAddSuc (NumS m) n  = EqlS $ addPOneCommAddSuc m n
