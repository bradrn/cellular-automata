{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}

module CA.Universe (module CA.Types, module CA.Universe) where

import Data.Functor ((<&>))

import Data.Array ((!), (//))
import qualified Data.Array as A

import CA.Core
import CA.Types

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

-- | Changes the value of a single 'Point' in a 'Universe'. If the
-- point is out of bounds, do nothing.
modifyPoint :: Point -> (a -> a) -> Universe a -> Universe a
modifyPoint p f un@(Universe u) =
    if (A.inRange (A.bounds u) p)
    then Universe u'
    else un
  where
    u' = u // [(p, f (u ! p))]

-- | Changes the value of a single 'Point' in a 'Universe'. If the
-- point is out of bounds, wrap around (like 'peek' does).
modifyPointWrap :: Point -> (a -> a) -> Universe a -> Universe a
modifyPointWrap (Point x y) f un@(Universe u) =
    let (width, height) = size un
        p' = Point (x `mod` width) (y `mod` height)
    in Universe $ u // [(p', f (u ! p'))]

-- * Conversion from and to lists

-- | Converts a list of rows to a 'Universe'. Assumes that each row is
-- the same length and that there is at least one row.
fromList :: [[a]] -> Universe a
fromList l =
    Universe $ A.array (Point 0 0,hi) assocs
  where
    hi = Point (Coord $ length (head l) - 1) (Coord $ length l - 1)

    assocs = concat $ imap (\y -> imap $ \x val -> (Point x y, val)) l
      where
        imap :: (Coord x -> a -> b) -> [a] -> [b]
        imap f = go 0
          where
            go n (x:xs) = (f n x):(go (n+1) xs)
            go _ []     = []

-- | Converts a 'Universe' to a list of rows.
toList :: Universe a -> [[a]]
toList (Universe u) =
    (fmap.fmap) (u !) (coords $ A.bounds u)
  where
    coords :: (Point, Point) -> [[Point]]
    coords (Point lo_x lo_y, Point hi_x hi_y) =
        [lo_y..hi_y] <&> (\y -> flip Point y <$> [lo_x..hi_x])

-- | Extracts a portion of a 'Universe' to a list of rows. Wraps around
-- toroidally if a point outside the edges of the universe has been requested.
clip :: Universe a -> Bounds -> [[a]]
clip u Bounds{..} = (fmap . fmap) (flip peek u) ps
  where
    -- Creates a nested array of Points, with the 2D index corresponding to the
    -- row and column at which the point is found
    ps = fmap (\mkPoint -> fmap mkPoint [boundsLeft..boundsRight]) $ fmap (flip Point) [boundsTop..boundsBottom]

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
