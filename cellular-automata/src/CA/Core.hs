{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}

{-|
Module     : CA
Copyright  : (c) Brad Neimann 2017-2018
License    : MIT
Maintainer : brad.neimann@hotmail.com

Main functionality relating to cellular automata.
-}
module CA.Core where

{- |

A typeclass for cellular automaton. An instance is of the form
@'CA' p c@, where @c@ is the type of the universe and @p@ is the type
of coordinates in the universe.

To specify the laws, we will first need an auxiliary definition: a
coordinate @p@ is said to be /in bounds of/ a universe @c@ if and
only if @peek p (evolve const c)@ is equal to @p@. The 'CA' laws are
then as follows:

- @evolve peek = id@
- @evolve g . evolve f = evolve (\p -> g p . evolve f)@
- For all @p@ and @c@, if @p@ is in bounds of @c@, then
  @peek p (evolve f c) = f p c@.

-}

class Functor c => CA p c | c -> p where
    -- | Given a coordinate and a universe, find the value at the
    -- given coordinate in the universe.
    peek   :: p     -- ^ Coordinate
           -> c a   -- ^ Universe
           -> a     -- ^ Result


    -- | The function used to apply a rule to a universe. The rule
    -- itself is specified as a function, which 'evolve' should apply
    -- to each value in the universe. This function should take the
    -- following parameters:
    --
    -- - The first parameter, of type @p@, is the coordinate the rule
    --   is currently being applied to.
    -- - The second parameter, of type @c a@, is the universe the rule
    --   is being applied to.
    --
    -- === __Example usage__
    --
    -- Imagine that we have an infinite 1D universe, with the
    -- coordinate type being 'Int'. We now want to create a rule to
    -- simply replace each value in a universe with the one next to
    -- it. Such a rule could take the form @\p ca -> peek (p-1) ca@,
    -- and would be applied to a universe by writing
    -- @evolve (\p ca -> peek (p-1) ca) universe@.
    evolve :: (p -> c a -> b)  -- ^ The rule function to apply
           -> c a              -- ^ The universe to apply it to
           -> c b              -- ^ The result of applying the rule to the universe

-- | The type of a cellular automaton update rule which will work on
-- any universe with coordinate type @p@ and value type @c@. For
-- details on the meaning of the parameters, see the documentation for
-- 'evolve'.
type CARule p a = forall c. CA p c => p -> c a -> a

-- | An applicative version of 'CARule'. Can be used to add effects to
-- rules; for instance, a rule with randomness
-- (via <https://hackage.haskell.org/package/MonadRandom MonadRandom>)
-- or logging.
type CARuleA t p a = forall c. CA p c => p -> c a -> t a

-- | Apply a 'CARuleA' to a 'Traversable' universe. Effects are
-- applied in 'Traversable' order.
evolveA :: (CA p c, Applicative t, Traversable c) => (p -> c a -> t a) -> c a -> t (c a)
evolveA rule = sequenceA . evolve rule

-- | Convert a 'CARule' to a 'CARuleA'.
pureRule :: Applicative t => CARule p a -> CARuleA t p a
pureRule r = \p ca -> pure $ r p ca
