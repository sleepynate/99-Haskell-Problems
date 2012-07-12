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

tests21_30 = [TestLabel "Problem 21" problem21]