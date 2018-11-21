{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

module Main where

import Criterion.Main

import CA
import CA.Internal (Universe(..))
import CA.Utils

main :: IO ()
main = defaultMain $
    [ bPatternsWith "trivial"    trivial
    , bPatternsWith "identity"   identity
    , bPatternsWith "conwayLife" conwayLife
    ]

bPatternsWith :: String -> (Universe Bool -> Bool) -> Benchmark
bPatternsWith name rule =
    bgroup name
        [ benchIters "block"      block      rule
        , benchIters "glider"     glider     rule
        , benchIters "rPentomino" rPentomino rule
        ]

benchIters :: String -> Universe Bool -> (Universe Bool -> Bool) -> Benchmark
benchIters name u rule = bgroup name $ benchn <$> [1, 100, 200, 300, 400, 500]
  where
    benchn :: Int -> Benchmark
    benchn n = bench (show n) $ nf (extendN n rule) u

extendN :: Comonad w => Int -> (w a -> a) -> (w a -> w a)
extendN n w = applyN (extend w)
  where
    applyN f = foldr (.) id (replicate n f)

-- | The trivial rule
trivial :: Comonad w => w Bool -> Bool
trivial = const False

-- | The identity rule
identity :: Comonad w => w a -> a
identity = extract

-- | Converts a list of rows to a 'Universe' of 'Bool's. Each row is
-- itself a list if integers, with @0@ being converted to 'False' and
-- @1@ converted to 'True'. Padding of width 14 is added to all sides
-- of the 'Universe' to get a good size in which to do complex
-- simulations.
mkGrid :: [[Int]] -> Universe Bool
mkGrid = fmap (==1) . fromList . expandSides 14 0

expandSides :: Int -> a -> [[a]] -> [[a]]
expandSides n val u = filler ++ (fmap addToSides u) ++ filler
  where
    filler = (replicate n $ replicate n val)
    addToSides r = (replicate n val) ++ r ++ (replicate n val)

block :: Universe Bool
block = mkGrid
    [[0,0,0]
    ,[0,1,1]
    ,[0,1,1]]

glider :: Universe Bool
glider = mkGrid
    [[1,1,1]
    ,[0,0,1]
    ,[0,1,0]]

rPentomino :: Universe Bool
rPentomino = mkGrid
    [[0,1,1]
    ,[1,1,0]
    ,[0,1,0]]
