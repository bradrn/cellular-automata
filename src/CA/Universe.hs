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
    
      -- * Conversion from and to lists
    , render
    , clip
    , clipInside
    ) where

import CA.Core
import CA.Universe.Internal
