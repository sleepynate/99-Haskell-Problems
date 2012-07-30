module Problems21_30 where

import Data.List
import Test.HUnit
import System.Random
import System.IO.Unsafe

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

problem21 :: Test
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

problem22 :: Test
problem22 = test [ "Create a list containing all integers in a given range."
                 ~: range 4 9
                 ~=? [4,5,6,7,8,9]]

{-
 - Problem 23
 - Extract a given number of randomly selected elements from a list.
 - Example:
 - Prelude System.Random>rnd_select "abcdefgh" 3 >>= show
 - "eda"
 -}

pure_rnd_select :: [a] -> Int -> [a]
pure_rnd_select x y = unsafePerformIO $ rnd_select x y

rnd_select :: [a] -> Int -> IO [a]
rnd_select _  0 = return []
rnd_select [] _ = return []
rnd_select l n = do r <- randomRIO (0, (length l) - 1)
                    result <- (rnd_select ((take r l) ++ (drop (r+1) l)) (n - 1))
                    return ((l!!r) : result)

problem23 :: Test
problem23 = test ["Extract given number of randomly selected elements from list"
                  ~: (length $ (pure_rnd_select "abcdefgh" 3))
                  ~=? 3 ]

{-
 - Problem 24
 - Lotto: Draw N different random numbers from the set 1..M.
 - Example:
 - Prelude System.Random>diff_select 6 49
 - [23,1,17,33,21,37]
 -}

diff_select :: Int -> Int -> [Int]
diff_select n r = pure_rnd_select [1..r] n

problem24 :: Test
problem24 = test ["Draw N different random number from the set 1..M"
                  ~: (length $ diff_select 6 49)
				  ~=? 6 ]
{-
 - Problem 25
 - Generate a random permutation of the elements of a list.
 - Example:
 - Prelude>rnd_permu "abcdef"
 - Prelude>"badcef"
 -}

rnd_permu :: Eq a => [a] -> [a]
rnd_permu [] = []
rnd_permu xs = x : rnd_permu (delete x xs)
               where x = head (pure_rnd_select xs 1)

problem25 :: Test
problem25 = test ["Generate a random permutation of the elements of a list."
                  ~: length (rnd_permu "abcdef")
				  ~=? length "abcdef" ]


{-
 - Problem 26
 - (**) Generate the combinations of K distinct objects chosen from
 -the  N elements of a list
 - In how many ways can a committee of 3 be chosen from a group of 12
 - people? We all know that there are C(12,3) = 220 possibilities
 - (C(N,K) denotes the well-known binomial coefficients). For pure
 - mathematicians,  this result may be great. But we want to really
 - generate all the possibilities in a list.
 -
 - Example:
 - > combinations 3 "abcdef"
 - ["abc","abd","abe",...]
 -}

{-
 - Problem 27
 - Group the elements of a set into disjoint subsets.
 - a) In how many ways can a group of 9 people work in 3 disjoint
 - subgroups of 2, 3 and 4 persons? Write a function that generates all
 - the possibilities and returns them in a list.
 - b) Generalize the above predicate in a way that we can specify a
 - list of group sizes and the predicate will return a list of groups.
 - Note that we do not want permutations of the group members;
 - i.e. ((ALDO BEAT) ...) is the same solution as ((BEAT ALDO) ...).
 - However, we make a difference between ((ALDO BEAT) (CARLA DAVID) ...)
 - and ((CARLA DAVID) (ALDO BEAT) ...).
 - You may find more about this combinatorial problem in a good book
 - on discrete mathematics under the term "multinomial coefficients".
 - Example:
 - P27> group [2,3,4] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
 - [[["aldo","beat"],["carla","david","evi"],["flip","gary","hugo","ida"]],...]
 - (altogether 1260 solutions)
 - 27> group [2,2,5] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
 - [[["aldo","beat"],["carla","david"],["evi","flip","gary","hugo","ida"]],...]
 - (altogether 756 solutions)
 -}

{- Problem 28
 - Sorting a list of lists according to length of sublists
 -
 - a) We suppose that a list contains elements that are lists
 - themselves. The objective is to sort the elements of this list
 - according to their length. E.g. short lists first, longer lists
 - later, or vice versa.
 - Example:
 - Prelude>lsort ["abc","de","fgh","de","ijkl","mn","o"]
 - > ["o","de","de","mn","abc","fgh","ijkl"]
 - b) Again, we suppose that a list contains elements that are lists
 - themselves. But this time the objective is to sort the elements of
 - this list according to their length frequency; i.e., in the
 - default, where sorting is done ascendingly, lists with rare lengths
 - are placed first, others with a more frequent length come later.
 -
 - Example:
 -
 - > lfsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
 - ["ijkl","o","abc","fgh","de","de","mn"]
 -
 -}

tests21_30 :: [Test]
tests21_30 = [TestLabel "Problem 21" problem21,
              TestLabel "Problem 22" problem22,
              TestLabel "Problem 23" problem23,
			  TestLabel "Problem 24" problem24,
			  TestLabel "Problem 25" problem25]
