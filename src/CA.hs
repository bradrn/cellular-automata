{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
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
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ViewPatterns               #-}

{-|
Module     : CA
Copyright  : (c) Brad Neimann 2017-2018
License    : MIT
Maintainer : brad.neimann@hotmail.com

Main functionality relating to cellular automata.
-}
module CA ( -- * About comonadic cellular automata
            -- $comonadic

            -- * Convenient re-exports
            module Control.Comonad
          , module Control.Comonad.Store.Class
          , module Control.Monad.Random.Strict
            -- * Types
          , Axis(..)
          , Coord(..)
          , Point(..)
          , Universe
          , StochRule
          , Bounds(..)
          , boundsWidth
          , boundsHeight
            -- * Functions on 'Universe'
          , CA.Internal.fromList
          , size
          , evolve
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

import CA.Internal
import Control.Comonad
import Control.Comonad.Store.Class
import Control.Monad.Random.Strict hiding (fromList)

-- $comonadic
--
-- == What are Comonads?
-- A 'Comonad' is a class which can be thought of as being the \'opposite' of a
-- 'Monad'. Whereas a 'Monad' can be thought of as encapsulating the notion of
-- an 'effectful computation' - that is, a computation which produces an effect
-- - a 'Comonad' can be thought of as encapsulating the notion of a 'contextual
-- computation' - that is, a computation which takes place in a context, which
-- can then be accessed during the computation. The methods of a 'Comonad' are
-- \'reversed' (or /dual/) compared to those of a 'Monad', as can be seen by
-- comparing them:
--
-- * @'return' :: 'Monad' m => a -> m a@ wraps a value in a monadic effect,
-- whereas @'extract' :: 'Comonad' w => w a -> a@ isolates a value from its
-- comonadic context
-- * @(>>=) :: 'Monad' m => m a -> (a -> m b) -> m b@ applies an effectful
-- computation @a -> m b@ to a value @m a@, whereas
-- @'flip' 'extend' :: 'Comonad' w => w a -> (w a -> b) -> w b@ applies a
-- contextual computation @w a -> b@ to a value @w a@.
--
-- The stereotypical example of a comonad is the /product comonad/, which is
-- just a partially applied tuple @(,) r@. This is the simplest comonad,
-- representing a value with another value of type @r@ acting as a context. If
-- you said that this sounds like the Reader monad you would be right; this
-- comonad is indeed isomorphic to the reader monad! In further detail:
--
-- * @'extract' :: (r, a) -> a@ removes the @r@ context to leave just the @a@
-- value; and
-- * @'extend' :: ((r, a) -> b) -> (r, a) -> (r, b)@ applies a contextual
-- computation @(r, a) -> b@ to a value @(r, a)@.
--
-- == Using Comonads to Simulate Cellular Automata
-- It turns out that comonads are a natural fit for cellular automata. The
-- trick is to figure out how to view a cellular automaton as a contextual
-- computation. If you have a look at a cellular automaton like Conway's Game
-- of Life, the rules all follow the same general pattern of 'take a cell, and
-- if the surrounding grid satisfies a certain condition then update the cell
-- as follows'. This already /is/ a contextful computation, in which the
-- current cell is updated with reference to the context of the rest of the
-- grid! This means that a cellular automaton comonad can be implemented as a
-- pair of a 2D grid and one \'focused' point, to provide the \'current cell'
-- referred to above (by the way, you might recognise this construction as a 2D
-- list zipper). This is implemented as the 'Universe' comonad below. A
-- cellular automaton can then be implemented as a function @'Universe' a ->
-- b@, the argument of which is pre-\'focused' to the cell which is currently
-- being recomputed. The 'extract' method can be used to get the focused value,
-- and the 'extend' method can be used to apply a cellular automaton to a
-- 'Universe'. Putting this all together, the Game of Life can be implemented as
-- follows:
--
-- @
-- data State = Alive | Dead
-- conwayLife :: 'Universe' State -> State
-- conwayLife u = case 'extract' u of                      -- We use 'extract' to get the currently focused value
--   Dead ->
--     if count ('experiment' mkNbhd u) == 3               -- 'experiment' is useful to get a neighbourhood from the focused cell
--     then Alive
--     else Dead
--   Alive ->
--     if count ('experiment' mkNbhd u) `elem` [2, 3]
--     then Alive
--     else Dead
--  where
--   mkNbhd (Point x y) = [ Point (x-1) (y-1)
--                        , Point  x    (y-1)
--                        , Point (x+1) (y-1)
--                        , Point (x-1)  y
--                        , Point (x+1)  y
--                        , Point (x-1) (y+1)
--                        , Point  x    (y+1)
--                        , Point (x+1) (y+1)
--                        ]
--  count = sum . fmap (\case { Alive -> 1 ; Dead -> 0 }) -- Note that this uses -XLambdaCase
-- @
--
-- Note that it would normally be much easier to use the functions from
-- "CA.Utils", but in the interests of pedagogy these have been avoided.
