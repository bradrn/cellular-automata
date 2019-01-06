{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}

{-|
Provides a set of reusable coordinate and universe types for use with
the 'CA' typeclass.
-}
module CA.Types
    ( -- * Re-exports
      module CA.Core

      -- * Coordinate Types
    , Axis(..)
    , Coord(..)
    , Point(..)

      -- * Universe Types
    , Universe(..)
    , size
    ) where

import GHC.Generics

import Data.Array ((!))
import qualified Data.Array as A
import Control.DeepSeq

import CA.Core

-- | A type used with -XDataKinds to parameterise 'Coord'.
data Axis = X | Y

-- | The type of one coordinate. Parameterised by a phantom type for
-- type-safety; ideally, each coordinate used to specify a point
-- should be given a separate type.
newtype Coord (x :: Axis) = Coord { getCoord :: Int }
    deriving (Show, Eq, Ord, Enum)
    deriving newtype (Num, Real, Integral, A.Ix)

-- | The type of a 2D point. Phantom types are used for type-safety.
data Point = Point (Coord 'X) (Coord 'Y) deriving (Show, Eq)

instance Ord Point where
    compare (Point x1 y1) (Point x2 y2) = compare (x1,y1) (x2,y2)

instance A.Ix Point where
    range (Point lo_x lo_y, Point hi_x hi_y) =
        uncurry Point <$> (A.range ((lo_x,lo_y), (hi_x,hi_y)))
    index (Point lo_x lo_y, Point hi_x hi_y) (Point x y) =
        A.index ((lo_x,lo_y), (hi_x,hi_y)) (x,y)
    inRange (Point lo_x lo_y, Point hi_x hi_y) (Point x y) =
        A.inRange ((lo_x,lo_y), (hi_x,hi_y)) (x,y)

-- | The type of a finite, toroidal, 2D universe of cells of type @a@.
--
-- __NOTE__: This is indexed using __positive integers__, so the top-left
-- point is (0, 0) and the bottom-right point is (/m/, /n/).
--
-- Internally, this is stored as a 2D array.
newtype Universe a = Universe (A.Array Point a)
    deriving (Eq, Show, Functor, Foldable, Traversable)

instance CA Point Universe where
    peek p un@(Universe u) = u ! (wrap p)
      where
        wrap (Point x y) =
            let (w, h) = size un
            in Point (x `mod` w) (y `mod` h)

    evolve fn u =
        let (w, h) = size u
            hi = Point (w-1) (h-1)
        in Universe $ fromFunction (Point 0 0,hi) (\p -> fn p u)
      where
        -- Given a set of bounds, construct an 'Array' by applying a
        -- function to each point in the bounds
        fromFunction :: A.Ix p => (p, p) -> (p -> a) -> A.Array p a
        fromFunction bounds f =
            let ps = A.range bounds
                vals = fmap (\p -> (p, f p)) ps
            in
                A.array bounds vals

deriving instance Generic Point
deriving instance Generic a => Generic (Universe a)

deriving newtype instance NFData (Coord c)
deriving instance NFData Point
deriving newtype instance NFData a => NFData (Universe a)

-- | Returns the width and height of a 'Universe'.
size :: Universe a -> (Coord 'X, Coord 'Y)
size (Universe u) =
    let (Point lo_x lo_y, Point hi_x hi_y) = A.bounds u
    in (hi_x-lo_x+1, hi_y-lo_y+1)
