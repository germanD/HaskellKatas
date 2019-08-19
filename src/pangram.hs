{- DetectPangram by anindyabd -}
{- https://www.codewars.com/kata/detect-pangram/train/haskell -}

{-

A pangram is a sentence that contains every single letter of the
alphabet at least once. For example, the sentence "The quick brown fox
jumps over the lazy dog" is a pangram, because it uses the letters A-Z
at least once (case is irrelevant).

Given a string, detect whether or not it is a pangram. Return True if
it is, False if not. Ignore numbers and punctuation.


-}

module Pangram where
import Data.Char
import Data.List
import Data.Set(toList,fromList)

-- A..Z ASCII range
-- length ['A'..'Z'] == 26

isPangram :: String -> Bool
isPangram xs =  length (collectASCII xs) == 26
  where collectASCII = toList . fromList . filter isAlpha. map toUpper
--- toList . fromList == nub, but with better performance
