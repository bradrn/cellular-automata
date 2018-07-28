{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE ViewPatterns               #-}

module Main where

import GHC.Generics (Generic)

import Control.DeepSeq (NFData)
import Criterion.Main
import System.Random

import CA
import CA.Internal (Universe(..))
import CA.ALPACA

deriving instance Generic Int
deriving instance Generic Point
deriving instance Generic a => Generic (Universe a)

deriving newtype instance NFData (Coord c)
deriving instance NFData Point
deriving instance (Generic a, NFData a) => NFData (Universe a)

main :: IO ()
main = defaultMain
    -- [ env (return $ fromList $ (1:replicate 499 0):(replicate 499 $ replicate 500 0)) $ \ca ->
    --     bgroup "quick CA" [brepn quickRule ca 200]
    [ bgroup "Haskell evolution function" $ bsize haskellLife <$> [50, 100, 200]
    , bgroup "ALPACA evolution function"  $ bsize alpacaLife  <$> [50, 100, 200]
    ]

bsize :: (Universe Int -> Rand StdGen (Universe Int)) -> Int -> Benchmark
bsize ev size = env (randomCA size) $ \ca ->
    bgroup (show size++"x"++show size) $ brepn ev ca <$> [1, 10, 50]

brepn :: NFData a => (a -> Rand StdGen a) -> a -> Int -> Benchmark
brepn f x n = let f' = evalRandIO . (foldr (<=<) return $ replicate n f)
              in bench (show n) $ nfIO (f' x)

randomCA :: Int -> IO (Universe Int)
randomCA size = (fromList . take size . splitEvery size . randomRs (0, 1)) <$> getStdGen
  where
    splitEvery n (splitAt n -> (x, xs)) = x:splitEvery n xs

quickRule :: Universe Int -> Rand g (Universe Int)
quickRule = evolve $ return . \g -> case extract g of
    1 -> 2
    2 -> 1
    _ -> 0

haskellLife :: Universe Int -> Rand g (Universe Int)
haskellLife = evolve $ return . \g ->
    let nbrs = sum $ experiment nbhd g in
        case extract g of
            0 -> if nbrs == 3          then 1 else 0
            1 -> if nbrs `elem` [2, 3] then 1 else 0
            _ -> error "Unexpected element!"
  where
    nbhd (Point x y) = [Point (x+dx) (y+dy) | dx <- [-1, 0, 1]
                                            , dy <- [-1, 0, 1]
                                            , not ((dx == 0) && (dy == 0))]

alpacaLife :: RandomGen g => Universe Int -> Rand g (Universe Int)
alpacaLife = evolve $ either error fst $ runALPACA
    "state Dead \" \" to Alive when 3 Alive and 5 Dead; state Alive \"*\" to Dead when 4 Alive or 7 Dead."
