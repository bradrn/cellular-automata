{-# OPTIONS_HADDOCK not-home #-}

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

import Control.Comonad
import Control.Comonad.Store.Class
import Control.Monad.Random.Strict

-- * Types

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

-- | Converts a 'StochRule' into a function updating @u a@.
evolve :: (ComonadStore p u, Traversable u) => StochRule u g a -> (u a -> Rand g (u a))
evolve ca = sequenceA . extend ca
