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

class Functor c => CA p c | c -> p where
    peek   :: p -> c a -> a
    evolve :: (p -> c a -> b) -> c a -> c b
{-
Laws:

(first defining `inBounds p c = peek p (evolve const c) == p`)

- evolve peek = id
- If `inBounds p c`, then `peek p (evolve f c) = f p c`
- evolve g . evolve f = evolve (\\p -> g p . evolve f)
-}

type CARule p a = forall c. CA p c => p -> c a -> a

type CARuleA t p a = forall c. CA p c => p -> c a -> t a

evolveA :: (CA p c, Applicative t, Traversable c) => (p -> c a -> t a) -> c a -> t (c a)
evolveA rule = sequenceA . evolve rule

pureRule :: Applicative t => CARule p a -> CARuleA t p a
pureRule r = \p ca -> pure $ r p ca
