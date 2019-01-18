# cellular-automata

`cellular-automata` is a flexible library for the simulation of cellular
automata (henceforth CA/CAs). It is based around a new typeclass which can
represent cellular automata with arbitrary topologies (introduced [here](https://www.reddit.com/r/haskell/comments/a2vnni/a_better_comonad_for_cellular_automata/)).
It also contains some data types for various kinds of CA universes, as well as
several utility functions.

## Examples

The examples below can be run by calling
`evolve _selectedRule _myUniverse`; this will evolve `_myUniverse` by
one generation according to the selected rule.

```haskell
-- Conway's Game of Life. (Included in the CA.Utils module)
conwayLife :: CARule Point Bool
-- Note the type signature above: it states that conwayLife operates
-- on universes which use 2D points ('Point'), with state type 'Bool'.
conwayLife curPoint curUniverse =
    case (peek curPoint curUniverse) of
        False -> if surrounds == 3          then True else False
        True  -> if surrounds `elem` [2, 3] then True else False
  where
    -- The number of surrounding 'True' cells
    surrounds = count id $
        fmap (`peek` cur_universe) $ moore False curPoint

-- Wolfram's Rule 30
rule30 :: CARule Int Bool
-- Again, the type signature states that rule30 operates on universes
-- which use 1D points ('Int'), with state type 'Bool'.
rule30 curPoint curUniverse =
    let left  = peek (p-1) s
        mid   = peek  p    s
        right = peek (p+1) s
    in
        (    left && not mid && not right)
     || (not left &&     mid &&     right)
     || (not left &&     mid && not right)
     || (not left && not mid &&     right)
```
