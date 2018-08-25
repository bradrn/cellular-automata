{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE ViewPatterns               #-}

{-|
Module     : CA.Internal
Copyright  : (c) Brad Neimann 2017-2018
License    : MIT
Maintainer : brad.neimann@hotmail.com

A module containing the internals of 'CA'. Do not use unless you /really/ need
to!
-}
module CA.Internal ( -- * Convenient re-exports
                     module Control.Comonad
                   , module Control.Comonad.Store.Class
                   , module Control.Monad.Random.Strict
                   , module CA.Internal
                   ) where

import Data.Foldable (toList)

import Control.Comonad
import Control.Comonad.Store.Class
import Control.Monad.Random.Strict hiding (fromList)
import qualified Data.Sequence as S

-- * Types

-- | A type used with -XDataKinds to parameterise 'Coord'.
data Axis = X | Y

newtype Coord (x :: Axis) = Coord { getCoord :: Int }
    deriving (Show, Eq, Ord, Num, Enum, Real, Integral)

data Point = Point (Coord X) (Coord Y) deriving (Show, Eq)

-- | The type of a finite, 2D universe of cells of type @a@.
--
-- __NOTE__: This is indexed using __positive integers__, so the top-left
-- point is (0, 0) and the bottom-right point is (/m/, /n/).
--
-- Internally, this is stored as a list of rows and a single \'focused' point,
-- to form a 2D zipper.
data Universe a = Universe (S.Seq (S.Seq a)) Point
    deriving (Show, Functor, Foldable, Traversable)

-- | This instance ignores the foci and just tests the playfields for equality.
instance Eq a => Eq (Universe a) where
    (Universe u1 _) == (Universe u2 _) = u1 == u2

-- | Allows the evolution of a cellular automaton using the 'extend' method.
instance Comonad Universe where
    extract (normalize -> Universe u (Point x y)) = S.index (S.index u (fromIntegral y)) (fromIntegral x)
    duplicate un@(Universe u p) = Universe u' p
      where
        (getCoord -> w, getCoord -> h) = size un
        u' = S.fromFunction h $ \y ->
             S.fromFunction w $ \x ->
             Universe u (Point (Coord x) (Coord y))

-- | Gives a few useful utility functions.
instance ComonadStore Point Universe where
    pos (normalize -> Universe _ p) = p
    peek p (Universe u _) = extract $ Universe u p
    peeks f grid@(pos -> p) = peek (f p) grid
    seek p (Universe u _) = Universe u p
    seeks f = seek <$> (f . pos) <*> id
    experiment f grid = fmap (flip peek grid) $ f $ pos grid

-- | A convenient type synonym for a non-stochastic cellular automaton update
-- rule, where @u@ is the comonadic container data type and @a@ is the state
-- type. It can be converted into a usable function using 'extend'.
type Rule u a = u a -> a

-- | A convenient type synonym for a stochastic cellular automaton update
-- function, where @u@ is the comonadic container data type, @g@ is the random
-- number generator, and @a@ is the state type. It can be converted into a
-- usable function using 'evolve'. For instance, @StochRule Point StdGen Int@
-- would be the type of a CA which acts on a universe with cells indexed by
-- 'Point', with each cell having type 'Int' and using a 'StdGen' random number
-- generator.
type StochRule u g a = u a -> Rand g a

-- | Specifies a portion of a 'Universe'.
data Bounds = Bounds
    { boundsLeft   :: !(Coord X)
    , boundsRight  :: !(Coord X)
    , boundsTop    :: !(Coord Y)
    , boundsBottom :: !(Coord Y)
    } deriving (Show)

-- | Finds the number of columns in a 'Bounds'.
boundsWidth :: Bounds -> Coord X
boundsWidth Bounds{..} = boundsRight - boundsLeft + 1

-- | Finds the number of rows in a 'Bounds'.
boundsHeight :: Bounds -> Coord Y
boundsHeight Bounds{..} = boundsBottom - boundsTop + 1

-- * Functions on 'Universe'

-- | Returns the width and height of a 'Universe'.
size :: Universe a -> (Coord 'X, Coord 'Y)
size (Universe u _) = (Coord $ width u, Coord $ S.length u)
  where
    width (v S.:<| _) = S.length v
    width _           = 0

-- | Wraps the coordinates of the focus to within the allowed range. Generally
-- you won't need to use this unless you're messing around with the internals of
-- a 'Universe'.
normalize :: Universe a -> Universe a
normalize un@(Universe u (Point x y)) = Universe u (Point x' y')
  where
    (w, h) = size un
    x' = x `mod` w
    y' = y `mod` h

-- | Converts a 'StochRule' into a function updating @u a@.
evolve :: (ComonadStore p u, Traversable u) => StochRule u g a -> (u a -> Rand g (u a))
evolve ca = sequenceA . extend ca

-- | Changes the value of a single 'Point' in a 'Universe'.
--
-- __TODO__ The behaviour is /undefined/ in the case when the point is
-- outside the edges of the 'Universe'. This /is/ a bug and will be fixed later.
modifyPoint :: Point -> (a -> a) -> Universe a -> Universe a
modifyPoint (Point x y) f (Universe u p) = Universe u' p
  where
    u' = S.adjust' (S.adjust' f (fromIntegral x)) (fromIntegral y) u

-- * Conversion from and to lists

-- | Converts a list of rows to a 'Universe'.
fromList :: [[a]] -> Universe a
fromList l = Universe (fmap S.fromList $ S.fromList $ l) (Point 0 0)

-- | Converts a 'Universe' to a list of rows.
render :: Universe a -> [[a]]
render (Universe u _) = toList $ fmap toList u

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
