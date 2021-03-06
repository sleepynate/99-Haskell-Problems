module Main where

import Test.Framework

import Test.HUnit
import qualified Problems1_10
import qualified Problems11_20
import qualified Problems21_30

import System.Exit

tests = Problems1_10.tests1_10 ++ Problems11_20.tests11_20 ++ Problems21_30.tests21_30

-- main::IO()
-- main = do
--     results <- runTestTT tests
--     let errs = errors results
--         fails = failures results
--     exitWith (codeGet errs fails)

main = defaultMain tests

codeGet errs fails
 | fails > 0       = ExitFailure 2
 | errs > 0        = ExitFailure 1
 | otherwise       = ExitSuccess
