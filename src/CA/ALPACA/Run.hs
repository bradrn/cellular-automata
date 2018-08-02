{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module CA.ALPACA.Run (Defns, run) where
import Data.Bool (bool)
import Data.List (find)
import Data.Maybe (fromMaybe)

import Control.Comonad
import Control.Comonad.Store
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Random.Strict as S
import Data.Finite
import Lens.Micro
import Lens.Micro.Mtl

import CA.ALPACA.Parse
import CA
import CA.Utils (Direction(..))

type Defns n = ([StateDefn (Finite n)], [ClassDefn], [NbhdDefn])
type DefnsM n g = R.ReaderT (Defns n) (S.Rand g)
type CARulePart n a g = Universe (Finite n) -> DefnsM n a g

run :: forall n g. RandomGen g => (Defns n) -> CARule g (Finite n)
run defns = flip R.runReaderT defns . run' defns
 where
   run' :: (Defns n) -> Universe (Finite n) -> DefnsM n g (Finite n)
   run' (stateDefns, _, _) = withDefault extract (runMany runStateDefn) stateDefns

runStateDefn :: RandomGen g => (StateDefn (Finite n)) -> CARulePart n g (Maybe (Finite n))
runStateDefn (StateDefn s _ _ classes rules) = \grid ->
    let s' = extract grid in
        if s == s' then do
            inheritedRules <- view _2 <&> flip collectRules classes
            runRules (rules ++ inheritedRules) grid
        else
            return Nothing

-- The following function is NOT in the DefnsM monad; it seems to force a value
-- too early, sometimes causing infinite loops when there shouldn't be any.
-- Plus, the code is actually simpler this way :)
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

runRules :: RandomGen g => [Rule] -> CARulePart n g (Maybe (Finite n))
runRules = runMany runRule

runRule :: RandomGen g => Rule -> CARulePart n g (Maybe (Finite n))
runRule (Rule sRef Nothing) = getRef sRef
runRule (Rule sRef (Just expr)) = \grid -> exprTrue expr grid >>= bool (return Nothing) (getRef sRef grid)

getRef :: StateRef -> CARulePart n g (Maybe (Finite n))
getRef Me = pure . Just . extract
getRef (StateID n) = const $
    withField _1 (\(StateDefn _ name _ _ _) -> n == name)
                 (\(StateDefn s _    _ _ _) -> s)
getRef (DirRef ds) = \grid -> pure $ Just $ flip peeksRel grid $ moves ds

exprTrue :: RandomGen g => Expression -> CARulePart n g Bool
exprTrue (ExprLog t op x) = \grid -> getOp op <$> termTrue t grid <*> exprTrue x grid
exprTrue (ExprLeaf t) = termTrue t

getOp :: LogOp -> Bool -> Bool -> Bool
getOp And = (&&)
getOp Or  = (||)
getOp Xor = (/=)

termTrue :: RandomGen g => Term -> CARulePart n g Bool
termTrue (AdjacencyPred n nbhd sRef) = \grid ->
    resolve (fromMaybe (Left moore) nbhd) >>= \case
        Just nbhd' ->
          let cells = explore nbhd' grid in
              (>=n) <$> count (flip (matches sRef) grid) cells
        Nothing -> return False
  where
    resolve :: Either Neighbourhood (Name 'NbhdType) -> DefnsM n g (Maybe Neighbourhood)
    resolve (Left nbhd') = return $ Just nbhd'
    resolve (Right name) = withField _3 (\(NbhdDefn name' _) -> name' == name) (\(NbhdDefn _ nbhd') -> nbhd')

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
termTrue (Not t) = (fmap not) . termTrue t
termTrue (BoolPrim prim) = boolPrim prim
termTrue (RelationalPred ref1 ref2) = \grid -> do
    getRef ref1 grid >>= \case
        Just ref1' -> matches ref2 ref1' grid
        Nothing    -> return False

boolPrim :: RandomGen g => BoolPrim -> CARulePart n g Bool
boolPrim BPTrue = const $ return True
boolPrim BPFalse = const $ return False
boolPrim BPGuess = const $ S.lift $ getRandom

matches :: Either StateRef (Name 'ClassType) -> (Finite n) -> CARulePart n g Bool
matches (Left s) s' = \grid -> getRef s grid <&> maybe False (==s')
matches (Right c) s = const $ inClass c s

explore :: Neighbourhood -> Universe (Finite n) -> [(Finite n)]
explore nbhd = \grid -> fmap (flip peeksRel grid . moves) nbhd

peeksRel :: Point -> Universe (Finite n) -> (Finite n)
peeksRel (Point x y) = peeks $ \(Point x' y') -> Point (x+x') (y+y')

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

runMany :: Monad m => (a -> (b -> m (Maybe c))) -> ([a] -> (b -> m (Maybe c)))
runMany f (r:rs) = \b -> do
    v <- f r b
    case v of
        Just v' -> return $ Just v'
        Nothing -> runMany f rs b
runMany _ [] = const $ return Nothing

withDefault :: Monad m => (b -> c) -> (a -> (b -> m (Maybe c))) -> (a -> (b -> m c))
withDefault d f  = \a b -> fromMaybe (d b) <$> f a b
