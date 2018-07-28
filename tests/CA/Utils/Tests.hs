module CA.Utils.Tests (tests) where

import Test.Tasty
import Test.Tasty.QuickCheck

import CA
import CA.Utils

-- To import Arbitrary instances
import CA.Tests ()

tests :: TestTree
tests = testGroup "CA.Utils"
    [ testProperty "moore length without center"      (prop_nbhd (moore      False) 8)
    , testProperty "moore length with center"         (prop_nbhd (moore      True ) 9)
    , testProperty "vonNeumann length without center" (prop_nbhd (vonNeumann False) 4)
    , testProperty "vonNeumann length with center"    (prop_nbhd (vonNeumann True ) 5)
    , testProperty "pointed returns right points" (\ps -> pointed ps (Point 0 0) == ps)
    ]

prop_nbhd :: (Point -> [Point]) -> Int -> (Universe Int -> Bool)
prop_nbhd nbhd n = \ca -> length (experiment nbhd ca) == n
