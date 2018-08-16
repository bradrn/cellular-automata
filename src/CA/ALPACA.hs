{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}

{-|
Module     : CA.ALPACA
Copyright  : (c) Brad Neimann 2017-2018
License    : MIT
Maintainer : brad.neimann@hotmail.com

Implements a parser for version 1.0 of the ALPACA language. Documentation is
available at <https://github.com/catseye/ALPACA/blob/fcd5a8bdd579c209475dedd2e221b8c437d5ca21/doc/ALPACA.markdown>
-}
module CA.ALPACA (runALPACA, AlpacaData(..), getRule) where

import Prelude hiding (lookup)

import Data.Bifunctor (second)
import Data.Maybe (fromJust)
import Data.Proxy
import Data.Tuple (swap)
import GHC.TypeLits

import Data.Finite
import Data.Map.Strict (Map, mapKeys, lookup, toList)
import Lens.Micro

import CA.ALPACA.Parse
import CA.ALPACA.Run
import CA

-- | The data specified by an ALPACA definition. Existentially quantified over
-- the number of states.
data AlpacaData g = forall n. KnownNat n =>
    AlpacaData { rule       :: CARule g (Finite n)                 -- ^ The rule itself
               , initConfig :: Maybe (Universe (Finite n))         -- ^ The initial configuration
               , stateData  :: Finite n -> (String, Maybe Char)    -- ^ Returns the name and representation declaration of each character
               , revLookup  :: Either String Char -> [Finite n]
                 -- ^ Reverse of 'stateData': given a name or a representation declaration, returns the associated state(s).
               }

-- | A convenience function to convert the rule in an 'AlpacaData' to a rule
-- with 'Integer' state. Note that if you give the generated 'CARule' a state
-- outside the bounds of the original rule, it emits an exception.
getRule :: AlpacaData g -> CARule g Integer
getRule (AlpacaData{rule=r}) = fmap getFinite . r . fmap finite

-- | Converts an ALPACA specification to a 'CARule'. The 'CARule' returned
-- operates on a 'Finite' state (existentially protected using 'SomeRule').
runALPACA :: forall g. RandomGen g => String -> Either String (AlpacaData g)
runALPACA = second go . parseALPACA
  where
    go :: (ALPACA, Map Int (String, Maybe Char)) -> AlpacaData g
    go (parsed@(ALPACA _ initConfig'), names) = case someNatVal (maxState parsed + 1) of
        Nothing ->
            error "Error - CA.ALPACA.runALPACA.go.maxState returned a negative number. This is a bug - please report to the package maintainer."
        Just (SomeNat (Proxy :: Proxy n)) ->
            let defns = extractDefns parsed :: Defns n
                rule  = run defns
                initConfig = (fmap . fmap) (finite . toInteger) initConfig'
                namesMap = mapKeys (finite . toInteger) names
                -- Each state SHOULD have a map entry, so it's fine using fromJust
                stateData = fromJust . (flip lookup namesMap)
                revLookup = search $ swap <$> toList namesMap
            in  AlpacaData{..}
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

        search :: (Eq a, Eq b) => [((a, Maybe b), c)] -> Either a b -> [c]
        search (((a', _      ), c):xs) s@(Left  a) | a == a' = c : (search xs s)
        search (((_ , Just b'), c):xs) s@(Right b) | b == b' = c : (search xs s)
        search (_                 :xs) s                     =      search xs s
        search [] _ = []
