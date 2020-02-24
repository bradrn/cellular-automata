{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module CA.Strip.Tests where

import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Data.Stream.Infinite.Skew as S
    
import CA.Core
import CA.Strip

instance Arbitrary a => Arbitrary (Strip a) where
    arbitrary = do
        -- Make two lists and a zero point, then concatenate them
        neg <- arbitrary @[a]
        pos <- arbitrary @[a]
        z   <- arbitrary @a
        zero <- arbitrary @a
        return $ Strip (toStream zero neg) z (toStream zero pos)
      where
        toStream :: a -> [a] -> S.Stream a
        toStream zero l = foldr (S.<|) (S.repeat zero) l

tests :: TestTree
tests = testGroup "CA"
    [ testGroup "Universe"
      [ testGroup "CA laws"
        [ testProperty "evolve peek         = id"
              (prop_ca1 @Strip @Int)
        , testProperty "peek p (evolve f c) = f p c     (when in bounds)"
              (prop_ca2 @Int @Int)
        , testProperty "evolve g . evolve f = evolve (\\p -> g p . evolve f)"
              (prop_ca3 @Strip @Int @Int @Int)
        ]
      ]
    ]

    
prop_ca1 :: forall c a p. (CA p c, Eq (c a)) => c a -> Bool
prop_ca1 c = c == (evolve peek c)

prop_ca2 :: forall a b. Eq b => Blind (Integer -> Strip a -> b) -> Integer -> Strip a -> Bool
prop_ca2 (Blind f) p c = peek p (evolve f c) == f p c

prop_ca3 :: forall u a b c p. (CA p u, Eq (u c)) => Blind (p -> u a -> b) -> Blind (p -> u b -> c) -> u a -> Bool
prop_ca3 (Blind f) (Blind g) c = evolve (\p -> g p . evolve f) c == (evolve g . evolve f) c
