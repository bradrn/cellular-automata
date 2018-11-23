{-# OPTIONS_GHC -fno-warn-orphans#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}

module CA.Universe.Tests (tests) where

import Control.Monad (replicateM)

import Test.Tasty
import Test.Tasty.QuickCheck

import CA.Universe

deriving instance Arbitrary (Coord a)
deriving instance CoArbitrary (Coord a)
instance Arbitrary Point where
    arbitrary = Point <$> arbitrary <*> arbitrary
    shrink (Point a b) = uncurry Point <$> shrink (a, b)
instance Arbitrary a => Arbitrary (Universe a) where
    arbitrary = sized $ \n -> resize (round $ sqrt $ fromIntegral @_ @Double n) $ do
        l <- arbitrary `suchThat` (>0)
        m <- arbitrary `suchThat` (>0)
        list <- replicateM l $ replicateM m arbitrary
        return $ fromList list
instance Arbitrary Bounds where
    arbitrary = do
        boundsLeft   <- getSmall <$> arbitrary
        boundsRight  <- getSmall <$> arbitrary
        boundsTop    <- getSmall <$> arbitrary
        boundsBottom <- getSmall <$> arbitrary
        return Bounds{..}

deriving instance Random (Coord x)

tests :: TestTree
tests = testGroup "CA"
    [ testGroup "Universe"
      [ testGroup "Comonad laws"
        [ testProperty "extract . duplicate      = id"                         (prop_comonad1 :: Universe Int -> Bool)
        , testProperty "fmap extract . duplicate = id"                         (prop_comonad2 :: Universe Int -> Bool)
        , testProperty "duplicate . duplicate    = fmap duplicate . duplicate" (prop_comonad3 :: Universe Int -> Bool)
        ]
      , testGroup "ComonadStore laws"
        [ testProperty "x = seek (pos x) x"           (prop_store1 :: Universe Int -> Bool)
        -- Note that Universe does not satisfy the second law, as it wraps the index to within the range
        -- , testProperty "y = pos (seek y x)"           (prop_store2 :: Universe Int -> Point -> Bool)
        , testProperty "seek y x = seek y (seek z x)" (prop_store3 :: Universe Int -> Point -> Point -> Bool)
        ]
      , testGroup "render and fromList"
        [ testProperty "render . fromList = id" (prop_render1 :: [[Int]] -> Bool)
        , testProperty "fromList . render = id" (prop_render2 :: Universe Int -> Bool)
        ]
      , testProperty "size works correctly" (prop_size :: NonEmptyList [Int] -> Bool)
      , testProperty "clipInside calculates new bounds correctly" (prop_clipInsideBounds :: Bounds -> Universe Int -> Bool)
      , testProperty "modifyPoint modifies correct point" (prop_modifyPoint :: Blind (Int -> Int) -> Universe Int -> Property)
      ]
    ]

prop_comonad1 :: (Arbitrary (c a), Comonad c, Eq (c a)) => c a -> Bool
prop_comonad1 c = c == (extract . duplicate $ c)

prop_comonad2 :: (Arbitrary (c a), Comonad c, Eq (c a)) => c a -> Bool
prop_comonad2 c = c == (fmap extract . duplicate $ c)

prop_comonad3 :: (Arbitrary (c a), Comonad c, Eq (c (c (c a)))) => c a -> Bool
prop_comonad3 c = (duplicate . duplicate $ c) == (fmap duplicate . duplicate $ c)

prop_store1 :: (Arbitrary (c a), ComonadStore i c, Eq (c a)) => c a -> Bool
prop_store1 x = x == seek (pos x) x

prop_store2 :: (Arbitrary (c a), Arbitrary i, ComonadStore i c, Eq i) => c a -> i -> Bool
prop_store2 x y = y == pos (seek y x)

prop_store3 :: (Arbitrary (c a), Arbitrary i, ComonadStore i c, Eq (c a)) => c a -> i -> i -> Bool
prop_store3 x y z = seek y x == seek y (seek z x)

prop_render1 :: (Arbitrary a, Eq a) => [[a]] -> Bool
prop_render1 u = render (fromList u) == u

prop_render2 :: (Arbitrary a, Eq a) => Universe a -> Bool
prop_render2 u = fromList (render u) == u

-- NonEmptyList is used for convenience so head won't error
prop_size :: NonEmptyList [a] -> Bool
prop_size (NonEmpty l) = (Coord w, Coord h) == size (fromList l)
  where
    w = length (head l)
    h = length l

prop_clipInsideBounds :: Arbitrary a => Bounds -> Universe a -> Bool
prop_clipInsideBounds bs u = let (bs', _) = clipInside u bs
                             in (boundsLeft   bs' >= boundsLeft   bs) &&
                                (boundsRight  bs' <= boundsRight  bs) &&
                                (boundsTop    bs' >= boundsTop    bs) &&
                                (boundsBottom bs' <= boundsBottom bs)

prop_modifyPoint :: Eq a => Blind (a -> a) -> Universe a -> Property
prop_modifyPoint (Blind f) u = forAll pointInRange $ \p -> peek p (modifyPoint p f u) == f (peek p u)
  where
    pointInRange :: Gen Point
    pointInRange =
        let (width, height) = size u
        in Point <$> choose (0, width-1) <*> choose (0, height-1)
