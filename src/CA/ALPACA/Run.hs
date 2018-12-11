{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module CA.ALPACA.Run (Defns, run) where
import Data.Bool (bool)
import Data.List (find)
import Data.Maybe (fromMaybe)

import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Random.Strict as S
import Data.Finite
import Lens.Micro
import Lens.Micro.Mtl

import CA.ALPACA.Parse
import CA.Core
import CA.Universe
import CA.Utils (Direction(..))

type Defns n = ([StateDefn (Finite n)], [ClassDefn], [NbhdDefn])
type DefnsM n g = R.ReaderT (Defns n) (S.Rand g)
type RulePart n a g = forall u. CA Point u => Point -> u (Finite n) -> DefnsM n a g

run :: forall n g. (S.RandomGen g) => (Defns n) -> CARuleA (S.Rand g) Point (Finite n)
run defns = \p -> flip R.runReaderT defns . run' defns p
 where
   run' :: (CA Point u) => (Defns n) -> Point -> u (Finite n) -> DefnsM n g (Finite n)
   run' (stateDefns, _, _) = withDefault peek (runMany runStateDefn) stateDefns

runStateDefn :: S.RandomGen g => (StateDefn (Finite n)) -> RulePart n g (Maybe (Finite n))
runStateDefn (StateDefn s _ _ classes rules) = \p grid ->
    let s' = peek p grid in
        if s == s' then do
            inheritedRules <- view _2 <&> flip collectRules classes
            runRules (rules ++ inheritedRules) p grid
        else
            return Nothing

collectRules :: [ClassDefn] -> [Name 'ClassType] -> [Rule]
collectRules classes = concatMap (go [])
  where
    go visited c =
        if c `elem` visited
        then []
        else case find isRightClass classes of
            Just (ClassDefn _ supers rules) -> rules ++ concatMap (go (c:visited)) supers
            Nothing -> []
      where
        isRightClass (ClassDefn name _ _) = c == name

runRules :: S.RandomGen g => [Rule] -> RulePart n g (Maybe (Finite n))
runRules = runMany runRule

runRule :: S.RandomGen g => Rule -> RulePart n g (Maybe (Finite n))
runRule (Rule sRef Nothing) = getRef sRef
runRule (Rule sRef (Just expr)) = \p grid -> exprTrue expr p grid >>= bool (return Nothing) (getRef sRef p grid)

getRef :: StateRef -> RulePart n g (Maybe (Finite n))
getRef Me = \p grid -> pure $ Just $ peek p grid
getRef (StateID n) = const $ const $
    withField _1 (\(StateDefn _ name _ _ _) -> n == name)
                 (\(StateDefn s _    _ _ _) -> s)
getRef (DirRef ds) = \p grid -> pure $ Just $ peeksRel grid p $ moves ds

exprTrue :: S.RandomGen g => Expression -> RulePart n g Bool
exprTrue (ExprLog t op x) = \p grid -> getOp op <$> termTrue t p grid <*> exprTrue x p grid
exprTrue (ExprLeaf t) = termTrue t

getOp :: LogOp -> Bool -> Bool -> Bool
getOp And = (&&)
getOp Or  = (||)
getOp Xor = (/=)

termTrue :: S.RandomGen g => Term -> RulePart n g Bool
termTrue (AdjacencyPred n nbhd sRef) = \p grid ->
    resolve (fromMaybe (Left moore) nbhd) >>= \case
        Just nbhd' ->
          let cells = explore nbhd' p grid in
              (>=n) <$> count (flip3 (matches sRef) p grid) cells
        Nothing -> return False
  where
    resolve :: Either Neighbourhood (Name 'NbhdType) -> DefnsM n g (Maybe Neighbourhood)
    resolve (Left nbhd') = return $ Just nbhd'
    resolve (Right name) = withField _3 (\(NbhdDefn name' _) -> name' == name) (\(NbhdDefn _ nbhd') -> nbhd')

    flip3 :: (a -> b -> c -> d) -> (b -> c -> a -> d)
    flip3 f = \c a b -> f b c a

    moore =
        [ [LeftDir]
        , [RightDir]
        , [UpDir]
        , [DownDir]
        , [UpDir,RightDir]
        , [UpDir,LeftDir]
        , [DownDir,RightDir]
        , [DownDir,LeftDir]
        ]

    count :: Applicative t => (a -> t Bool) -> [a] -> t Int
    count p = fmap sum . traverse (fmap (bool @Int 0 1) . p)
termTrue (ExpressionTerm expr) = exprTrue expr
termTrue (Not t) = \p -> (fmap not) . termTrue t p
termTrue (BoolPrim prim) = boolPrim prim
termTrue (RelationalPred ref1 ref2) = \p grid -> do
    getRef ref1 p grid >>= \case
        Just ref1' -> matches ref2 ref1' p grid
        Nothing    -> return False

boolPrim :: S.RandomGen g => BoolPrim -> RulePart n g Bool
boolPrim BPTrue  = const $ const $ return True
boolPrim BPFalse = const $ const $ return False
boolPrim BPGuess = const $ const $ S.lift $ S.getRandom

matches :: Either StateRef (Name 'ClassType) -> (Finite n) -> RulePart n g Bool
matches (Left s) s' = \p grid -> getRef s p grid <&> maybe False (==s')
matches (Right c) s = const $ const $ inClass c s

explore :: CA Point u => Neighbourhood -> Point -> u (Finite n) -> [(Finite n)]
explore nbhd = \p grid -> fmap (peeksRel grid p . moves) nbhd

peeksRel :: CA Point u => u (Finite n) -> Point -> Point -> (Finite n)
peeksRel grid (Point x y) (Point x' y') = peek (Point (x+x') (y+y')) grid

moves :: [Direction] -> Point
moves = foldr move (Point 0 0)
  where
    move LeftDir  (Point x y) = Point (x-1) y
    move RightDir (Point x y) = Point (x+1) y
    move UpDir    (Point x y) = Point x (y-1)
    move DownDir  (Point x y) = Point x (y+1)

inClass :: Name 'ClassType -> (Finite n) -> DefnsM n g Bool
inClass c s = maybe False (c `elem`) <$>
    withField _1 (\(StateDefn s' _ _ _ _) -> s' == s) (\(StateDefn _ _ _ cs _) -> cs)

withField :: Lens' a [b] -> (b -> Bool) -> (b -> c) -> R.ReaderT a (S.Rand g) (Maybe c)
withField l p f = do
    bs <- view l
    return $ case find p bs of
        (Just b) -> Just $ f b
        Nothing -> Nothing

runMany :: Monad m => (a -> (p -> b -> m (Maybe c))) -> ([a] -> (p -> b -> m (Maybe c)))
runMany f (r:rs) = \p b -> do
    v <- f r p b
    case v of
        Just v' -> return $ Just v'
        Nothing -> runMany f rs p b
runMany _ [] = const $ const $ return Nothing

withDefault :: Monad m => (p -> b -> c) -> (a -> (p -> b -> m (Maybe c))) -> (a -> (p -> b -> m c))
withDefault d f  = \a p b -> fromMaybe (d p b) <$> f a p b
