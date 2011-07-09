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
        "last element of [1,2,3,4]" ~: 4 ~=? (myLast [1,2,3,4]),
        "last element of ['x','y','z']" ~: 'z' ~=? (myLast "xyz")
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
        "second to last element of [1..4]" ~: 3 ~=? (myButLast [1..4]),
        "second to last element of ['a'..'z']" ~: 'y' ~=? (myButLast "xyz")
        ]
                        
{-
 - Problem 3
 - Find the K'th element of a list. The first element in the list is number 1.
 - 
 - Example in Haskell:
 - Prelude> elementAt [1,2,3] 2
 - 2
 - Prelude> elementAt "haskell" 5
 - 'e'
 -}

elementAt :: [a] -> Int -> a
elementAt series index = if index == 1
        then head series
        else elementAt (tail series) (index - 1)

problem3 = test [
        "second element of [1,2,3]" ~: 2 ~=? (elementAt [1,2,3] 2),
        "fifth element of \"haskell\"" ~: 'e' ~=? (elementAt "haskell" 5)
        ]

{- 
 - Problem 4
 - Find the number of elements of a list.
 - 
 - Example in Haskell:
 - 
 - Prelude> myLength [123, 456, 789]
 - 3
 - Prelude> myLength "Hello, world!"
 - 13
 -}

myLength :: [a] -> Int
myLength x = myLength' 0 x
        where
            myLength' i (x:xs) = myLength' (i + 1) xs
            myLength' i _      = i

problem4 = test [
        "length of [123,456,789] is 3" ~: 3 ~=? (myLength [123,456,789]),
        "length of \"Hello, world!\" is 13" ~: 13 ~=? (myLength "Hello. world!")
        ]


tests = TestList [TestLabel "Problem 1" problem1,
                  TestLabel "Problem 2" problem2,
                  TestLabel "Problem 3" problem3,
                  TestLabel "Problem 4" problem4
                  ]

main::IO()
main = (runTestTT tests) >>= (\x -> putStr $ show x)
