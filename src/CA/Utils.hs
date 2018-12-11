{-# LANGUAGE FlexibleContexts #-}

{-|

Module     : CA.Utils
Copyright  : (c) Brad Neimann 2017-2018
License    : MIT
Maintainer : brad.neimann@hotmail.com

Utility functions relating to cellular automata.
-}
module CA.Utils where

import Data.Bool (bool)

import CA.Core
import CA.Universe (Point(..))

-- * Neighbourhoods
--
-- $nbhds
--
-- For definitions of the neighbourhoods below, see <https://en.wikipedia.org/wiki/Moore_neighborhood>
-- and <https://en.wikipedia.org/wiki/Von_Neumann_neighborhood>.

-- | Generates a Moore neighbourhood for use with 'experiment'.
moore :: Bool                -- ^ Whether to include the central point
      -> (Point -> [Point])
moore center = (\p -> if center then id else filter (/=p)) <*>  -- Filter out center point
               pointed [ Point x y
                       | x <- [-1..1]
                       , y <- [-1..1]
                       ]

-- | Generates a von Neumann neighbourhood for use with 'experiment'.
vonNeumann :: Bool                -- ^ Whether to include the central point
           -> (Point -> [Point])
vonNeumann center = pointed $ (if center then [Point 0 0] else []) ++ [Point 0 1, Point (-1) 0, Point 0 (-1), Point 1 0]

-- | Turns a list of points into a neighbourhood which can be used with
-- 'experiment'.
pointed :: [Point] -> (Point -> [Point])
pointed ps = \(Point x y) -> [ Point (x+dx) (y+dy) | Point dx dy <- ps ]

experiment :: (Functor f, CA p c) => (p -> f p) -> p -> c a -> f a
experiment f p c = flip peek c <$> f p

-- | Counts the number of values in a 'Foldable' satisfying a condition. Useful
-- with neighbourhoods e.g. counting the number of live cells in a Moore
-- neighbourhood using @'count' (==Alive) . 'experiment' ('moore' 'False')@
count :: (Functor t, Foldable t) => (a -> Bool) -> t a -> Int
count p = sum . fmap (bool 0 1 . p)

-- * Other functions

-- | Conway's Game of Life.
conwayLife :: CARule Point Bool
conwayLife p g =
    let surrounds = count id (fmap (`peek` g) $ (moore False) p) in
        case peek p g of
            False -> if surrounds == 3          then True else False
            True  -> if surrounds `elem` [2, 3] then True else False

-- | A direction - either left, right, up or down. Each direction is suffixed
-- with @Dir@ to avoid confusion with 'Left' or 'Right'.
data Direction = LeftDir | RightDir | DownDir | UpDir deriving (Show)

-- | Translates a 'Point' by a 'Direction'.
move :: Direction -> Point -> Point
move LeftDir  (Point x y) = Point (x-1) y
move RightDir (Point x y) = Point (x+1) y
move UpDir    (Point x y) = Point x (y-1)
move DownDir  (Point x y) = Point x (y+1)
