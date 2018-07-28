{-|
Module     : CA.ALPACA
Copyright  : (c) Brad Neimann 2017-2018
License    : MIT
Maintainer : brad.neimann@hotmail.com

Implements a parser for version 1.0 of the ALPACA language. Documentation is
available at <https://github.com/catseye/ALPACA/blob/fcd5a8bdd579c209475dedd2e221b8c437d5ca21/doc/ALPACA.markdown>
-}
module CA.ALPACA (runALPACA) where

import Control.Arrow ((&&&))
import Data.Bifunctor (second)

import Lens.Micro

import CA.ALPACA.Parse
import CA.ALPACA.Run
import CA

-- | Converts an ALPACA specification to a 'CARule'. The 'CARule' returned
-- operates on an 'Int' state; the allowed values are specified in the second
-- element of the returned tuple.
--
-- __TODO:__ 'Data.Finite.Finite' from the 'finite-typelits' package might be a
-- better fit for the state than 'Int'.
runALPACA :: RandomGen g => String -> Either String (CARule g Int, [Int])
runALPACA = second ((run &&& getStates) . extractDefns) . parseALPACA
  where
    getStates (ss, _, _) = fmap getState ss
      where
        getState (StateDefn s _ _ _ _) = s

    extractDefns :: ALPACA -> Defns
    extractDefns (ALPACA ds _) = foldr insertDefn ([], [], []) ds
      where
        insertDefn :: Defn -> Defns -> Defns
        insertDefn (StateDefn' d) = over _1 (d:)
        insertDefn (ClassDefn' d) = over _2 (d:)
        insertDefn (NbhdDefn'  d) = over _3 (d:)
