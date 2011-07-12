module Problems1_10 where

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
        "second element of [1,2,3]" ~: 2 ~=? (elementAt [1,2,3] 2)
        , "fifth element of \"haskell\"" ~: 'e' ~=? (elementAt "haskell" 5)
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

{- Problem 5
 - Reverse a list.
 - 
 - Example in Haskell:
 - Prelude> reverse "A man, a plan, a canal, panama!"
 - "!amanap ,lanac a ,nalp a ,nam A"
 - Prelude> reverse [1,2,3,4]
 - [4,3,2,1]
 -}

myreverse :: [a] -> [a]
myreverse [] = []
myreverse (x:xs) = (myreverse xs) ++ [ x ]

problem5 = test [
        "reverse a string"
            ~: "!amanap ,lanac a ,nalp a ,nam A"
            ~=? (myreverse "A man, a plan, a canal, panama!"),
        "reverse a list of number"
            ~: [4,3,2,1]
            ~=? (myreverse [1,2,3,4])
        ]

{- 
 - Problem 6
 - Find out whether a list is a palindrome. A palindrome can be read
 - forward or backward; e.g. (x a m a x).
 - 
 - Example in Haskell:
 - *Main> isPalindrome [1,2,3]
 - False
 - *Main> isPalindrome "madamimadam"
 - True
 - *Main> isPalindrome [1,2,4,8,16,8,4,2,1]
 - True
 -}
isPalindrome :: Eq a => [a] -> Bool
isPalindrome (x:xs) = if (length xs <= 1) then True
                    else (x == last xs) && (isPalindrome (init xs))

problem6 = test [
        "check a list"
            ~: False ~=? (isPalindrome [1,2,3])
        , "check a string"
            ~: True ~=? (isPalindrome "madamimadam")
        , "check a longer list"
            ~: True ~=? (isPalindrome [1,2,4,8,16,8,4,2,1])
        ] 
{- 
 - Problem 7
 - Flatten a nested list structure.
 - 
 - Transform a list, possibly holding lists as elements into a `flat'
 - list by replacing each list with its elements (recursively).
 - 
 - Example in Haskell:
 - 
 - *Main> flatten (Elem 5)
 - [5]
 - *Main> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
 - [1,2,3,4,5]
 - *Main> flatten (List [])
 - []
 -}
data NestedList a = Elem a | List [NestedList a]

myflatten :: NestedList a -> [a]
myflatten (Elem x) = [x]
myflatten (List (x:xs)) = myflatten x ++ myflatten (List xs)
myflatten (List []) = []

problem7 = test [
        "flatten a 1-element list"
            ~: [5] ~=? myflatten (Elem 5)
        , "flatten a multi-depth list"
            ~: [1,2,3,4,5] ~=? myflatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
        -- , "flattening empty list is empty list"
        --     ~: [] ~=? myflatten (List [])
        ]
{- 
 - Problem 8
 - Eliminate consecutive duplicates of list elements.
 - If a list contains repeated elements they should be replaced with a single
 - copy of the element. The order of the elements should not be changed.
 - 
 - Example in Haskell:
 - 
 - > compress ["a","a","a","a","b","c","c","a","a","d","e","e","e","e"]
 - ["a","b","c","a","d","e"]
 -}
compress :: Eq a => [a] -> [a]
compress (x:[]) = [x]
compress (x:xs) = if x == head xs
                then compress xs
                else x:(compress xs)

problem8 = test [
        "remove duplicates from an iterable container"
        ~: ["a","b","c","a","d","e"] ~=? compress ["a","a","a","a","b","c","c","a","a","d","e","e","e","e"]
        ]
 
{- Problem 9
 - Pack consecutive duplicates of list elements into sublists. If a list
 - contains repeated elements they should be placed in separate sublists.
 - 
 - Example in Haskell:
 - *Main> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 
 -              'a', 'd', 'e', 'e', 'e', 'e']
 - ["aaaa","b","cc","aa","d","eeee"]
 -}

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = ([x] ++ takeWhile (== x) xs) : pack (dropWhile (== x) xs)

problem9 = test [
        "pack conecutive duplicates of list elements into sublists"
        ~: ["aaaa","b","cc","aa","d","eeee"]
        ~=? pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
        ]
 
{- Problem 10
 - Run-length encoding of a list. Use the result of problem P09 to implement
 - the so-called run-length encoding data compression method. Consecutive
 - duplicates of elements are encoded as lists (N E) where N is the number of
 - duplicates of the element E.
 - 
 - Example in Haskell:
 - encode "aaaabccaadeeee"
 - [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
 -}

encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode xs = encode' (pack xs)
        where
        encode' [] = []
        encode' (x:xs) = ((length x), (head x)) : encode' xs

problem10 = test [
        "run-length encode a list of duplicates"
        ~: [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
        ~=? encode "aaaabccaadeeee"
        ]

tests1_10 = TestList [TestLabel "Problem 1" problem1,
                  TestLabel "Problem 2" problem2,
                  TestLabel "Problem 3" problem3,
                  TestLabel "Problem 4" problem4,
                  TestLabel "Problem 5" problem5,
                  TestLabel "Problem 6" problem6,
                  TestLabel "Problem 7" problem7,
                  TestLabel "Problem 8" problem8,
                  TestLabel "Problem 9" problem9,
                  TestLabel "Problem 10" problem10
                  ]
