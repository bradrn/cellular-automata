{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}

module CA.Universe (module CA.Types, module CA.Universe) where

import Data.Foldable (toList)

import qualified Data.Sequence as S

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
