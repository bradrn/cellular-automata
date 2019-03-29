{-# OPTIONS_GHC -fno-warn-orphans#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}

module CA.Universe.Tests (tests) where

import Control.Monad (replicateM)

import System.Random (Random)
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Instances.Array ()

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

deriving newtype instance CoArbitrary a => CoArbitrary (Universe a)

instance CoArbitrary Point where
    coarbitrary (Point x y) = coarbitrary (x, y)

deriving instance Random (Coord x)

tests :: TestTree
tests = testGroup "CA"
    [ testGroup "Universe"
      [ testGroup "CA laws"
        [ testProperty "evolve peek         = id"
              (prop_ca1 @Universe @Int)
        , testProperty "peek p (evolve f c) = f p c     (when in bounds)"
              (prop_ca2 @Int @Int)
        , testProperty "evolve g . evolve f = evolve (\\p -> g p . evolve f)"
              (prop_ca3 @Universe @Int @Int @Int)
        ]
      , testGroup "render and fromList"
        -- Commented out until we figure out how to generate a list with each row the same length
        -- [ testProperty "render . fromList = id" (prop_render1 :: [[Int]] -> Bool)
        [ testProperty "fromList . render = id" (prop_render2 :: Universe Int -> Bool)
        ]
      -- Commented out until we figure out how to generate a list with each row the same length
      -- , testProperty "size works correctly" (prop_size :: NonEmptyList [Int] -> Bool)
      , testProperty "clipInside calculates new bounds correctly" (prop_clipInsideBounds :: Bounds -> Universe Int -> Bool)
      , testGroup "modifying points"
        [ testProperty "modifyPoint modifies correct point"
              (prop_modifyPoint :: Blind (Int -> Int) -> Universe Int -> Property)
        , testProperty "modifyPoint doesn't modify correct points"
              (prop_don'tModifyPoint :: Point -> Universe Int -> Property)
        , testProperty "modifyPointWrap modifies correct point"
              (prop_modifyPointWrap :: Point -> Blind (Int -> Int) -> Universe Int -> Bool)
        ]
      ]
    ]

prop_ca1 :: forall c a p. (CA p c, Eq (c a)) => c a -> Bool
prop_ca1 c = c == (evolve peek c)

-- Unlike all the other laws, this one is not polymorphic over the
-- universe type; this is because it must only generate points which
-- in bounds, but the method of doing this is different for every
-- universe type.
prop_ca2 :: forall a b. Eq b => Blind (Point -> Universe a -> b) -> Universe a -> Property
prop_ca2 (Blind f) c =
    forAll genInBounds $ \p ->
        (peek p (evolve f c) == f p c)
  where
    genInBounds =
        let (w, h) = size c in
                Point
            <$> choose (0, w-1)
            <*> choose (0, h-1)

prop_ca3 :: forall u a b c p. (CA p u, Eq (u c)) => Blind (p -> u a -> b) -> Blind (p -> u b -> c) -> u a -> Bool
prop_ca3 (Blind f) (Blind g) c = evolve (\p -> g p . evolve f) c == (evolve g . evolve f) c

-- prop_render1 :: (Arbitrary a, Eq a) => [[a]] -> Bool
-- prop_render1 u = render (fromList u) == u

prop_render2 :: (Arbitrary a, Eq a) => Universe a -> Bool
prop_render2 u = fromList (render u) == u

-- prop_size :: NonEmptyList [a] -> Bool
-- prop_size (NonEmpty l) = (Coord w, Coord h) == size (fromList l)
--   where
--     w = length (head l)
--     h = length l

prop_clipInsideBounds :: Arbitrary a => Bounds -> Universe a -> Bool
prop_clipInsideBounds bs u = let (bs', _) = clipInside u bs
                             in (boundsLeft   bs' >= boundsLeft   bs) &&
                                (boundsRight  bs' <= boundsRight  bs) &&
                                (boundsTop    bs' >= boundsTop    bs) &&
                                (boundsBottom bs' <= boundsBottom bs)

prop_modifyPointWrap :: Eq a => Point -> Blind (a -> a) -> Universe a -> Bool
prop_modifyPointWrap p (Blind f) u = peek p (modifyPointWrap p f u) == f (peek p u)

prop_modifyPoint :: Eq a => Blind (a -> a) -> Universe a -> Property
prop_modifyPoint (Blind f) u = forAll pointInRange $ \p -> peek p (modifyPoint p f u) == f (peek p u)
  where
    pointInRange :: Gen Point
    pointInRange =
        let (width, height) = size u
        in Point <$> choose (0, width-1) <*> choose (0, height-1)

prop_don'tModifyPoint :: (Eq a, Num a) => Point -> Universe a -> Property
prop_don'tModifyPoint p u =
    pointNotInRange p ==> peek p (modifyPoint p (+1) u) == peek p u
  where
    pointNotInRange :: Point -> Bool
    pointNotInRange (Point x y) =
        let (width, height) = size u
        in (x < 0) || (x >= width) || (y < 0) || (y >= height)
