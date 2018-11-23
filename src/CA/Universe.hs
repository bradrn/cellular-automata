module CA.Universe
    ( module CA.Core
    
    , Axis(..)
    , Coord(..)
    , Point(..)
    , Universe
    , Bounds(..)
    , boundsWidth
    , boundsHeight

      -- * Functions on 'Universe'
    , CA.Universe.Internal.fromList
    , size
    , modifyPoint
    
      -- ** Other functions which Haddock can't document properly
      --
      -- | @'extend' :: ('Universe' a -> b) -> 'Universe' a -> 'Universe' b@
      --
      -- This function can be used to evolve a 'Universe' by one generation using a
      -- non-stochastic evolution function.
      --
      -- @'extract' :: 'Universe' a -> a@
      --
      -- Returns the current focused point.
      --
      -- @'experiment' :: 'Functor' f => ('Point' -> f 'Point') -> 'Universe' a -> f a@
      --
      -- Given a function which turns the current focused point into a functor (e.g. a
      -- list) of points, 'experiment' returns the values at these points. Useful with
      -- the neighbourhood functions from "CA.Utils".
    
      -- * Conversion from and to lists
    , render
    , clip
    , clipInside
    ) where

import CA.Core
import CA.Universe.Internal
