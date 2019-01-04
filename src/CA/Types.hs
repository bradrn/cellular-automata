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

import Control.DeepSeq
import qualified Data.Sequence as S

import CA.Core

-- | A type used with -XDataKinds to parameterise 'Coord'.
data Axis = X | Y

-- | The type of one coordinate. Parameterised by a phantom type for
-- type-safety; ideally, each coordinate used to specify a point
-- should be given a separate type.
newtype Coord (x :: Axis) = Coord { getCoord :: Int }
    deriving (Show, Eq, Ord, Enum)
    deriving newtype (Num, Real, Integral)

-- | The type of a 2D point. Phantom types are used for type-safety.
data Point = Point (Coord 'X) (Coord 'Y) deriving (Show, Eq)

-- | The type of a finite, toroidal, 2D universe of cells of type @a@.
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

deriving instance Generic Point
deriving instance Generic a => Generic (Universe a)

deriving newtype instance NFData (Coord c)
deriving instance NFData Point
deriving newtype instance NFData a => NFData (Universe a)

-- | Returns the width and height of a 'Universe'.
size :: Universe a -> (Coord 'X, Coord 'Y)
size (Universe u) = (Coord $ width u, Coord $ S.length u)
  where
    width (v S.:<| _) = S.length v
    width _           = 0
