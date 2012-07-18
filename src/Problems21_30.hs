module Problems21_30 where

import Test.HUnit

{-
 - Problem 21
 - Insert an element at a given position into a list.
 - Example:
 - > insertAt 'X' "abcd" 2
 - "aXbcd"
 -}

insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n = take i xs ++ x : drop i xs
                  where i = n - 1

problem21 = test ["Insert an element at a given position into a list."
                 ~: insertAt 'X' "abcd" 2
                 ~=? "aXbcd"]

{-
 - Problem 22
 - Create a list containing all integers within a given range.
 - Example:
 - Prelude> range 4 9
 - [4,5,6,7,8,9]
 -}

range :: Int -> Int -> [Int]
range a b = if a == b
            then [b]
            else a : (range (a + 1) b)

problem22 = test [ "Create a list containing all integers in a given range."
                 ~: range 4 9
                 ~=? [4,5,6,7,8,9]]



tests21_30 = [TestLabel "Problem 21" problem21,
              TestLabel "Problem 22" problem22]