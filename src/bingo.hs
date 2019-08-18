
{- Scott Encoding Katta by A.Partridge -}
{- https://www.codewars.com/kata/lottery-ticket/train/haskell/ -}

{-

Time to win the lottery!

Given a lottery ticket (ticket), represented by an array of 2-value
arrays, you must find out if you've won the jackpot. Example ticket:

[ [ 'ABC', 65 ], [ 'HGR', 74 ], [ 'BYHT', 74 ] ]

To do this, you must first count the 'mini-wins' on your ticket. Each
sub array has both a string and a number within it. If the character
code of any of the characters in the string matches the number, you
get a mini win. Note you can only have one mini win per sub array.

Once you have counted all of your mini wins, compare that number to
the other input provided (win). If your total is more than or equal to
(win), return 'Winner!'. Else return 'Loser!'.

All inputs will be in the correct format. Strings on tickets are not
always the same length.

-}

module Bingo where

import Data.Char(ord)
import Data.Bool

{- Using the auxiliary miniWins as defined below, then 
   map miniWins tkts would compute a list of booleans, deciding 
   each mini-win. The next step is to count them using sum, which
   first requires mapping fromEnum to transform T and F to 0 and 1.
   
   The result would be something like this,

 bingo :: [(String,Int)] -> Int -> String
 bingo tkts n =
  let cnt = sum $  map (fromEnum . miniWins) tkts
       in if cnt >= n then "Winner!" else "Loser!"
    where miniWins :: (String,Int) -> Bool
          miniWins (p,q) = any ((==q) . ord) p
          
 The final version unfolds sum, and performs fold-map fusion to
 traverse the Ticket only once.

-}

bingo :: [(String,Int)] -> Int -> String
bingo tkts n =
  let cnt = foldr (\ p s -> s + fromEnum ( miniWins p)) 0 tkts
  in if cnt >= n then "Winner!" else "Loser!"
  where miniWins :: (String,Int) -> Bool
        miniWins (p,q) = any ((==q) . ord) p
