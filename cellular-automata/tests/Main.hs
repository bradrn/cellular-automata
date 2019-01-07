module Main where

import Test.Tasty (defaultMain, testGroup)

import qualified CA.Universe.Tests
import qualified CA.Format.MCell.Tests
import qualified CA.Utils.Tests

main :: IO ()
main = defaultMain $ testGroup "Tests"
       [ CA.Universe.Tests.tests
       , CA.Format.MCell.Tests.tests
       , CA.Utils.Tests.tests
       ]
