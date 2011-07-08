module Main where

import HUnit
{-
Problem 1
Find the last element of a list.

Example in Haskell:

Prelude> myLast [1,2,3,4]
4
Prelude> myLast ['x','y','z']
'z'
-}

myLast :: [a] -> a 
myLast (x:[]) = x
myLast (x:xs) = myLast xs


main::IO()
main = undefined

