{- Odd + Even = Odd? Prove it! by AJFarmar -}
{- https://www.codewars.com/kata/odd-plus-even-equals-odd-prove-it/train/haskell -}

{-
What's this kata about?

In this kata, you will prove, via types multiple facts about even and
odd numbers. These facts include:

If n is even, then n+1 is odd.
If n is odd, then n+1 is even.

If n is even and m is odd, then n+m is odd.

...and so on.

This will require knowledge of type families, datakinds, and GADTs.

How do we prove something with types?

Functions can be read as implications. Think of the type A -> B as
reading 'if we have an A, then we can get a B'. For this reason, if we
think of types as propositions rather than data, then we can build
proofs using functions between them, where A -> B is read as 'A
implies B'.

In this kata, three datatypes are defined:

-- | The natural numbers.
data Nat = Z | S Nat

-- | The axioms of the even numbers.
data Even (n :: Nat) where
  -- | Axiom: zero is even.
  ZeroEven :: Even Z
  -- | Axiom: if n is even, then n+2 is even.
  NextEven :: Even n -> Even (S (S n))

-- | The axioms of the odd numbers.
data Odd (n :: Nat) where
  -- | Axiom: one is odd.
  OneOdd :: Odd (S Z)
  -- | Axiom: if n is odd, then n+2 is odd.
  NextOdd :: Odd n -> Odd (S (S n))

Now we have the axioms built. Here they are represented as data
constructors, but in the corresponding proof we can imagine them as
the base assumptions from which we can build our proofs.

Once we have type families for certain operations built, we can build
proofs like so:

evenPlusOdd :: Even n -> Odd m -> Odd (Add m n) evenPlusOdd = --
(proof here) The initial solution will provide type signatures for all
the proofs you will need to complete. Good luck!

(Remember: the principle challenge in this kata is getting it to
typecheck. The rest is easy, as long as you don't use undefined.)

-}

{-# LANGUAGE GADTs, DataKinds, TypeFamilies, UndecidableInstances #-}

module OddsAndEvens where

-- | The natural numbers.
data Nat = Z | S Nat
  deriving (Eq , Show)

-- | The axioms of even numbers.
data Even (a :: Nat) :: * where
  -- | Zero is even.
  ZeroEven :: Even Z
  -- | If n is even, then n+2 is even.
  NextEven :: Even n -> Even (S (S n))

-- | The axioms of odd numbers.
data Odd (a :: Nat) :: * where
  -- | One is odd.
  OneOdd :: Odd (S Z)
  -- | If n is odd, then n+2 is odd.
  NextOdd :: Odd n -> Odd (S (S n))

-- | Proves that if n is even, n+1 is odd.
-- Notice how I use the axioms here.
evenPlusOne :: Even n -> Odd (S n)
evenPlusOne ZeroEven = OneOdd
evenPlusOne (NextEven n) = NextOdd (evenPlusOne n)

-- | Proves that if n is odd, n+1 is even.
oddPlusOne :: Odd n -> Even (S n)
oddPlusOne = error "TODO: oddPlusOne"

-- | Adds two natural numbers together.
-- Notice how the definition pattern matches.
type family   Add (n :: Nat) (m :: Nat) :: Nat
type instance Add Z m = m
type instance Add (S n) m = S (Add n m)

-- | Proves even + even = even
-- Notice how the pattern matching mirrors `Add`s definition.
evenPlusEven :: Even n -> Even m -> Even (Add n m)
evenPlusEven ZeroEven m = m
evenPlusEven (NextEven n) m = NextEven (evenPlusEven n m)

-- | Proves odd + odd = even
oddPlusOdd :: Odd n -> Odd m -> Even (Add n m)
oddPlusOdd = error "TODO: oddPlusOdd"

-- | Proves even + odd = odd
evenPlusOdd :: Even n -> Odd m -> Odd (Add n m)
evenPlusOdd = error "TODO: evenPlusOdd"

-- | Proves odd + even = odd
oddPlusEven :: Odd n -> Even m -> Odd (Add n m)
oddPlusEven = error "TODO: oddPlusEven"

-- | Multiplies two natural numbers.
type family   Mult (n :: Nat) (m :: Nat) :: Nat
type instance Mult Z m = Z

-- I'd wish to be able to write here
-- type instance Mult (S Z) m = m

-- as well, but type instances do not allow casing/if: they have to be
-- deterministic? 

type instance Mult (S n) m = Add m (Mult n m)

-- | Proves even * even = even
evenTimesEven :: Even n -> Even m -> Even (Mult n m)
evenTimesEven = error "TODO: evenTimesEven"

-- | Proves odd * odd = odd
oddTimesOdd :: Odd n -> Odd m -> Odd (Mult n m)
oddTimesOdd = error "TODO: oddTimesOdd"

-- | Proves even * odd = even
evenTimesOdd :: Even n -> Odd m -> Even (Mult n m)
evenTimesOdd = error "TODO: evenTimesOdd"

-- | Proves odd * even = even
oddTimesEven :: Odd n -> Even m -> Even (Mult n m)
oddTimesEven = error "TODO: oddTimesEven"
