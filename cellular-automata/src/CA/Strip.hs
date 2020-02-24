{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module CA.Strip where

import CA.Core

import qualified Data.Stream.Infinite.Skew as S

data Strip a = Strip (S.Stream a) a (S.Stream a)
    deriving (Show, Functor)

instance CA Integer Strip where
    peek p (Strip neg z pos)
        | p == 0 = z
        | p < 0 = neg S.!! abs p
        | p > 0 = pos S.!! p
        | otherwise = error "Arithmetic error in peek for strip"

    evolve f s =
        Strip (fmap (\i -> f (-i) s) $ S.from 0)
              (f 0 s)
              (fmap (\i -> f i s) $ S.from 0)

repeat :: a -> Strip a
repeat a = zero a a

zero :: a -> a -> Strip a
zero a z = Strip (S.repeat a) z (S.repeat a)

widthToList :: Integer -> Strip a -> [a]
widthToList w (Strip neg z pos) =
    reverse (takeS w neg) ++ [z] ++ (takeS w pos)
  where
    takeS :: Integer -> S.Stream a -> [a]
    takeS n = fmap snd . fst . S.split (\(i, _) -> i > n) . S.indexed

wolfram :: (Num n, CA n s) => Int -> (n -> s Bool -> Bool)
wolfram n = case padTo8 (toBinary n) of
    [d0,d1,d2,d3,d4,d5,d6,d7] -> \i s ->
        case (peek (i-1) s, peek i s, peek (i+1) s) of
           (False, False, False) -> d0
           (False, False, True ) -> d1
           (False, True , False) -> d2
           (False, True , True ) -> d3
           (True , False, False) -> d4
           (True , False, True ) -> d5
           (True , True , False) -> d6
           (True , True , True ) -> d7
    _ -> error "Wolfram rule number is out of range"
  where
     -- | Converts an integer to binary.
     -- Least significant bit is first.
     toBinary :: Int -> [Bool]
     toBinary x =
         case (x `quotRem` 2) of
             (0, r) -> [r==1]
             (q, r) -> (r==1) : toBinary q
    
     padTo8 [] = [False,False,False,False,False,False,False,False]
     padTo8 [d1] = [d1,False,False,False,False,False,False,False]
     padTo8 [d1,d2] = [d1,d2,False,False,False,False,False,False]
     padTo8 [d1,d2,d3] = [d1,d2,d3,False,False,False,False,False]
     padTo8 [d1,d2,d3,d4] = [d1,d2,d3,d4,False,False,False,False]
     padTo8 [d1,d2,d3,d4,d5] = [d1,d2,d3,d4,d5,False,False,False]
     padTo8 [d1,d2,d3,d4,d5,d6] = [d1,d2,d3,d4,d5,d6,False,False]
     padTo8 [d1,d2,d3,d4,d5,d6,d7] = [d1,d2,d3,d4,d5,d6,d7,False]
     padTo8 l = l
