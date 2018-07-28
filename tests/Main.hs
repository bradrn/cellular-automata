module Main where

import Test.Tasty (defaultMain, testGroup)

import qualified CA.Tests
import qualified CA.Format.MCell.Tests
import qualified CA.Utils.Tests

main :: IO ()
main = defaultMain $ testGroup "Tests"
       [ CA.Tests.tests
       , CA.Format.MCell.Tests.tests
       , CA.Utils.Tests.tests
       ]
