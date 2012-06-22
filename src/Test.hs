module Main where

import Test.HUnit
import qualified Problems1_10
import qualified Problems11_20

import System.Exit

tests = TestList (Problems1_10.tests1_10 ++ Problems11_20.tests11_20)

main::IO()
main = do
    results <- runTestTT tests
    let errs = errors results
        fails = failures results
    exitWith (codeGet errs fails)

codeGet errs fails
 | fails > 0       = ExitFailure 2
 | errs > 0        = ExitFailure 1
 | otherwise       = ExitSuccess
