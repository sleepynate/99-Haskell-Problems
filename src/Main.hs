module Main where

import Test.HUnit
import qualified Problems1_10

tests = Problems1_10.tests1_10

main::IO()
main = (runTestTT tests) >>= (\x -> putStr $ show x)
