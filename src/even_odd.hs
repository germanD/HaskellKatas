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
evenPlusOne (NextEven n) = NextOdd $ evenPlusOne n

-- | Proves that if n is odd, n+1 is even.
oddPlusOne :: Odd n -> Even (S n)
oddPlusOne OneOdd = NextEven ZeroEven
oddPlusOne (NextOdd n) = NextEven $ oddPlusOne n

-- | Adds two natural numbers together.
-- Notice how the definition pattern matches.
type family   Add (n :: Nat) (m :: Nat) :: Nat
type instance Add Z     m = m
type instance Add (S n) m = S (Add n m)

-- | Proves even + even = even
-- Notice how the pattern matching mirrors `Add`s definition.
evenPlusEven :: Even n -> Even m -> Even (Add n m)
evenPlusEven ZeroEven     = id
evenPlusEven (NextEven n) = NextEven . evenPlusEven n

-- | Proves odd + odd = even
oddPlusOdd :: Odd n -> Odd m -> Even (Add n m)
oddPlusOdd OneOdd      = oddPlusOne 
oddPlusOdd (NextOdd n) = NextEven . oddPlusOdd n

-- GAD: some add0n, addSn lemmas to avoid pattern-nmatching later

zeroPlusOdd :: Odd n -> Odd (Add Z n)
zeroPlusOdd OneOdd      = OneOdd
zeroPlusOdd (NextOdd n) = NextOdd $ zeroPlusOdd n

onePlusEven :: Even n -> Odd (Add (S Z) n)
onePlusEven ZeroEven     = OneOdd
onePlusEven (NextEven n) = NextOdd $ onePlusEven n

-- GAD: notice these are just a traversal with the constructors: in
-- regular (not-type-level) programming they would be just the
-- identity, here we have an effect in which we compute the proof: the
-- pattern above is a base constructor switch in the base inductive
-- case and a constructor applied to the IH.

-- However, now we change the base case to use the lemma above, but we
-- keep the constructor after IH nature of the inductive case.

-- | Proves even + odd = odd
evenPlusOdd :: Even n -> Odd m -> Odd (Add n m)
evenPlusOdd ZeroEven     = zeroPlusOdd
evenPlusOdd (NextEven n) = NextOdd . evenPlusOdd n


-- | Proves odd + even = odd
oddPlusEven :: Odd n -> Even m -> Odd (Add n m)

-- GAD: In an ideal world, one just write

-- oddPlusEven n m = evenPlusOdd m n

-- plus some rewriting on the type level, and then let the compiler
-- figure out that the types commute. But, proving indeed that Add or
-- Mult commute at the type level is hard (see the following
-- katas). Thus, we cannot escape doing some weight lifting.

oddPlusEven OneOdd      = onePlusEven
oddPlusEven (NextOdd n) = NextOdd . oddPlusEven n

-- | Multiplies two natural numbers.
type family   Mult (n :: Nat) (m :: Nat) :: Nat
type instance Mult Z m = Z
type instance Mult (S n) m = Add m (Mult n m)

-- | Proves even * even = even

-- I would (twice) need this lemma for adding evens twice
addTwiceLEven :: Even m -> Even n -> Even (Add m (Add m n))
addTwiceLEven m n = evenPlusEven m $ evenPlusEven m n

evenTimesEven :: Even n -> Even m -> Even (Mult n m)
evenTimesEven ZeroEven     m = ZeroEven
evenTimesEven (NextEven n) m = addTwiceLEven m $ evenTimesEven n m

-- GAD: For the next one, I'd wish to be able to write here

-- type instance Mult (S Z) m = m

-- as well, and be done with it... but type instances do not allow
-- casing/if. So, we will need a timesSn lemma.

oneTimesOdd :: Odd n -> Odd (Mult (S Z) n)
oneTimesOdd OneOdd      = OneOdd
oneTimesOdd (NextOdd n) = NextOdd $ oneTimesOdd n

oneTimesEven :: Even n -> Even (Mult (S Z) n)
oneTimesEven ZeroEven     = ZeroEven
oneTimesEven (NextEven n) = evenPlusEven two $ oneTimesEven n
   where two = NextEven ZeroEven
         
-- | Proves odd * odd = odd
oddTimesOdd :: Odd n -> Odd m -> Odd (Mult n m)
oddTimesOdd OneOdd      m = oneTimesOdd m
oddTimesOdd (NextOdd n) m = oddPlusEven m $ oddPlusOdd m $ oddTimesOdd n m

-- GAD: The proof takes the induction hypothesis (oddTimesOdd n m),
-- then applies oddPlusOdd m to it, and finally applies
-- oddPlusEven. In Coq/ssreflect we would (approximately) write:

-- exact (oddPlusOdd m (oddPlusEven m IH))

-- or even, move:IH; apply/(OddPlusEven m IH)/(odPlusOdd m).

-- | Proves even * odd = even
evenTimesOdd :: Even n -> Odd m -> Even (Mult n m)
evenTimesOdd ZeroEven     m = ZeroEven
evenTimesOdd (NextEven n) m = oddPlusOdd m $ oddPlusEven m $ evenTimesOdd n m

-- | Proves odd * even = even
oddTimesEven :: Odd n -> Even m -> Even (Mult n m)
oddTimesEven OneOdd      m = oneTimesEven m
oddTimesEven (NextOdd n) m = addTwiceLEven m $ oddTimesEven n m

-- Alternatively, we could case on the second, Even, argument, but
-- that would require other massaging lemmas to cope with the fact
-- that the type family instances are defined recursively on the first
-- argument.


{- Auxiliary definitions from the kata's test file:

-- Representations to Integers
fromEven :: Even n -> Int
fromEven ZeroEven = 0
fromEven (NextEven n) = 2 + fromEven n
fromOdd :: Odd n -> Int
fromOdd OneOdd = 1
fromOdd (NextOdd n) = 2 + fromOdd n

-- Numbers for help during tests
zero = ZeroEven
one = OneOdd
two = NextEven zero
three = NextOdd one
four = NextEven two
five = NextOdd three
six = NextEven four
seven = NextOdd five
eight = NextEven six
nine = NextOdd seven
ten = NextEven eight

-}
