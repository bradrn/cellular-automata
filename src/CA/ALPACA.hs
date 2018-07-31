{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}

{-|
Module     : CA.ALPACA
Copyright  : (c) Brad Neimann 2017-2018
License    : MIT
Maintainer : brad.neimann@hotmail.com

Implements a parser for version 1.0 of the ALPACA language. Documentation is
available at <https://github.com/catseye/ALPACA/blob/fcd5a8bdd579c209475dedd2e221b8c437d5ca21/doc/ALPACA.markdown>
-}
module CA.ALPACA (runALPACA, SomeRule(..)) where

import Data.Bifunctor (second)
import Data.Proxy
import GHC.TypeLits

import Data.Finite
import Lens.Micro

import CA.ALPACA.Parse
import CA.ALPACA.Run
import CA

-- | A 'CARule' with 'Finite' state type. However, the type @n@ is only known at
-- run-time, not compile-time, so it's existentially quantified.
data SomeRule g = forall n. KnownNat n => SomeRule (CARule g (Finite n))

-- | Converts an ALPACA specification to a 'CARule'. The 'CARule' returned
-- operates on a 'Finite' state (existentially protected using 'SomeRule').
runALPACA :: forall g. RandomGen g => String -> Either String (SomeRule g)
runALPACA = second go . parseALPACA
  where
    go :: ALPACA -> SomeRule g
    go parsed = case someNatVal (maxState parsed + 1) of
        Nothing ->
            error "Error - CA.ALPACA.runALPACA.go.maxState returned a negative number. This is a bug - please report to the package maintainer."
        Just (SomeNat (Proxy :: Proxy n)) ->
            let defns = extractDefns parsed :: Defns n
                fn    = run defns
            in  SomeRule fn
      where
        maxState :: ALPACA -> Integer
        maxState (ALPACA ds _) = foldr maxDefn 0 ds
          where
            maxDefn :: Defn -> Integer -> Integer
            maxDefn (StateDefn' (StateDefn s _ _ _ _)) i = max (toInteger s) i
            maxDefn _ i = i

        extractDefns :: forall n. KnownNat n => ALPACA -> Defns n
        extractDefns (ALPACA ds _) = foldr insertDefn ([], [], []) ds
          where
            insertDefn :: Defn -> Defns n -> Defns n
            insertDefn (StateDefn' d) = let d' = fmap fromIntegral d in over _1 (d':)
            insertDefn (ClassDefn' d) = over _2 (d:)
            insertDefn (NbhdDefn'  d) = over _3 (d:)
