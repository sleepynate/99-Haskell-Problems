module Main where

import Test.HUnit
import qualified Problems1_10
import qualified Problems11_20

tests = TestList (Problems1_10.tests1_10 ++ Problems11_20.tests11_20)

main::IO()
main = (runTestTT tests) >>= (\x -> putStrLn $ show x)
