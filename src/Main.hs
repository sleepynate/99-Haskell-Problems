module Main where

import Test.HUnit
{-
 - Problem 1
 - Find the last element of a list.
 -
 - Example in Haskell:
 - Prelude> myLast [1,2,3,4]
 - 4
 - Prelude> myLast ['x','y','z']
 - 'z'
 -}

myLast :: [a] -> a 
myLast (x:[]) = x
myLast (x:xs) = myLast xs

problem1 = test [
        "test1" ~: "last element of [1,2,3,4]" ~: 4 ~=? (myLast [1,2,3,4]),
        "test2" ~: "last element of ['x','y','z']" ~: 'z' ~=? (myLast "xyz")
        ]


{-
 - Problem 2
 - Find the last but one element of a list.
 - 
 - Example in Haskell:
 - Prelude> myButLast [1,2,3,4]
 - 3
 - Prelude> myButLast ['a'..'z']
 - 'y'
 -}
myButLast :: [a] -> a
myButLast (y:z:[]) = y
myButLast (y:zs)   = myButLast zs

problem2 = test [
        "test1" ~: "second to last element of [1..4]" ~: 3 ~=? (myButLast [1..4]),
        "test2" ~: "second to last element of ['a'..'z']" ~: 'y' ~=? (myButLast "xyz")
        ]
                        
tests = TestList [TestLabel "Problem 1" problem1,
                  TestLabel "Problem 2" problem2
                  ]

main::IO()
main = (runTestTT tests) >>= (\x -> putStr $ show x)