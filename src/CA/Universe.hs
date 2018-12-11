{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}

module CA.Universe
    ( module CA.Core
    , module CA.Universe
    ) where

import GHC.Generics
import Data.Foldable (toList)

import Control.DeepSeq
import qualified Data.Sequence as S

import CA.Core

-- | A type used with -XDataKinds to parameterise 'Coord'.
data Axis = X | Y

newtype Coord (x :: Axis) = Coord { getCoord :: Int }
    deriving (Show, Eq, Ord, Enum)
    deriving newtype (Num, Real, Integral)

data Point = Point (Coord 'X) (Coord 'Y) deriving (Show, Eq)

-- | The type of a finite, 2D universe of cells of type @a@.
--
-- __NOTE__: This is indexed using __positive integers__, so the top-left
-- point is (0, 0) and the bottom-right point is (/m/, /n/).
--
-- Internally, this is stored as a list of rows.
newtype Universe a = Universe (S.Seq (S.Seq a))
    deriving (Eq, Show, Functor, Foldable, Traversable)

instance CA Point Universe where
    peek p un@(Universe u) =
        let Point x y = wrap p in
            S.index (S.index u (fromIntegral y)) (fromIntegral x)
      where
        wrap (Point x y) =
            let (w, h) = size un
            in Point (x `mod` w) (y `mod` h)

    evolve fn u =
        let (w, h) = size u
        in Universe $
            fromFunction' h $ \row ->
                fromFunction' w $ \col ->
                    fn (Point col row) u
      where
        -- A variant of S.fromFunction using Coord for better type
        -- safety
        fromFunction' :: Coord x -> (Coord x -> a) -> S.Seq a
        fromFunction' len f = S.fromFunction (getCoord len) (f . Coord)

-- Generic instances

deriving instance Generic Point
deriving instance Generic a => Generic (Universe a)

deriving newtype instance NFData (Coord c)
deriving instance NFData Point
deriving newtype instance NFData a => NFData (Universe a)

-- | Specifies a portion of a 'Universe'.
data Bounds = Bounds
    { boundsLeft   :: !(Coord 'X)
    , boundsRight  :: !(Coord 'X)
    , boundsTop    :: !(Coord 'Y)
    , boundsBottom :: !(Coord 'Y)
    } deriving (Show)

-- | Finds the number of columns in a 'Bounds'.
boundsWidth :: Bounds -> Coord 'X
boundsWidth Bounds{..} = boundsRight - boundsLeft + 1

-- | Finds the number of rows in a 'Bounds'.
boundsHeight :: Bounds -> Coord 'Y
boundsHeight Bounds{..} = boundsBottom - boundsTop + 1

-- * Functions on 'Universe'

-- | Returns the width and height of a 'Universe'.
size :: Universe a -> (Coord 'X, Coord 'Y)
size (Universe u) = (Coord $ width u, Coord $ S.length u)
  where
    width (v S.:<| _) = S.length v
    width _           = 0


-- | Changes the value of a single 'Point' in a 'Universe'.
--
-- __TODO__ The behaviour is /undefined/ in the case when the point is
-- outside the edges of the 'Universe'. This /is/ a bug and will be fixed later.
modifyPoint :: Point -> (a -> a) -> Universe a -> Universe a
modifyPoint (Point x y) f (Universe u) = Universe u'
  where
    u' = S.adjust' (S.adjust' f (fromIntegral x)) (fromIntegral y) u

-- * Conversion from and to lists

-- | Converts a list of rows to a 'Universe'.
fromList :: [[a]] -> Universe a
fromList l = Universe (fmap S.fromList $ S.fromList $ l)

-- | Converts a 'Universe' to a list of rows.
render :: Universe a -> [[a]]
render (Universe u) = toList $ fmap toList u

-- | Extracts a portion of a 'Universe' to a list of rows. Wraps around
-- toroidally if a point outside the edges of the universe has been requested.
clip :: Universe a -> Bounds -> [[a]]
clip u Bounds{..} = (fmap . fmap) (flip peek u) ps
  where
    -- Creates a nested array of Points, with the 2D index corresponding to the
    -- row and column at which the point is found
    ps = fmap (\mkPoint -> fmap mkPoint [boundsTop..boundsBottom]) $ fmap Point [boundsLeft..boundsRight]

-- | Similar to 'clip', but instead of 'wrapping around' toroidally if a point
-- outside the edges of the universe has been requested, it stops at the edge of
-- the universe. This means that the list returned may not cover all of the
-- bounds requested, so 'clipInside' also returns the bounds which were actually
-- returned. For example, if the universe extends from the point (0,0) to (20,
-- 20) but @'Bounds'{'boundsLeft'=15, 'boundsRight'=25, 'boundsTop'=15,
-- 'boundsBottom'=25}@ was requested, then the portion outside the universe will
-- be ignored and 'clipInside' will behave as if only @'Bounds'{'boundsLeft'=15,
-- 'boundsRight'=20, 'boundsTop'=15, 'boundsBottom'=20}@ was requested, and will
-- return this value as the first part of the tuple.
clipInside :: Universe a -> Bounds -> (Bounds, [[a]])
clipInside u Bounds{..} = (bs, clip u bs)
  where
    (w, h) = size u
    bs = Bounds
        { boundsLeft = max boundsLeft 0
        , boundsTop  = max boundsTop  0
        , boundsRight  = min boundsRight  (w-1)
        , boundsBottom = min boundsBottom (h-1)
        }
