{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}

module CA.Format.MCell.Tests (tests) where

import Data.Char (ord)

import Data.ByteString.Lazy.Char8 (pack)
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.QuickCheck

import CA
import CA.Format.MCell
-- For the Arbitrary instances
import CA.Tests ()

deriving instance Eq Game
deriving instance Eq Coloring
deriving instance Eq MCell

instance Arbitrary Game where
    arbitrary = arbitraryBoundedEnum
instance Arbitrary Coloring where
    arbitrary = arbitraryBoundedEnum
instance Arbitrary MCell where
    arbitrary = MCell <$> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> ((fmap getNonNegative) <$> arbitrary)
                      <*> listOf (((,) <$> listOf (arbitraryASCIIChar `suchThat` (\c -> ord c `elem` [65..90])) -- generate only uppercase Chars
                                       <*> listOf arbitraryPrintableChar))

tests :: TestTree
tests = testGroup "CA.Format.MCell"
    [ testGroup "Encoding"
      [ testCase "Encodes glider correctly" "glider" $ encodeMCell $
            (mcell (fromList [[0,1,0],[0,0,1],[1,1,1]]) []){game=Just Life, rule=Just "B3/S23"}
      , testCase "Encodes block correctly" "block" $ encodeMCell $
            (mcell (fromList [[1,1],[1,1]]) []){game=Just Life, rule=Just "B3/S23"}
      , testCase "Compresses repetition correctly" "compress" $ encodeMCell $
            (mcell (fromList $ replicate 100 (replicate 100 1)) [])
      , testCase "Strips dead cells from end of lines" "strip" $ encodeMCell $
            (mcell (fromList $ [[1,1,1],[1,1,0],[1,0,0],[0,0,0]]) [])
      , testCase "Encodes all states correctly" "states" $ encodeMCell $
            (mcell (fromList $ [[0..256]]) [])
      , testCase "Encodes all options correctly" "options" $ encodeMCell MCell
            { game = Just Generations
            , rule = Just "/2/3"
            , speed = Just 2000
            , ccolors = Just 3
            , coloring = Just Standard
            , wrap = Just True
            , palette = Just "standard palette"
            , description = Just "Brian's Brain automaton\nEncoded by CA.Format.MCell"
            , universe = fromList [[0,1,0,0],[0,2,2,1],[1,2,2,0],[0,0,1,0]]
            , diversities = [("SYSTEM","act=1"),("NOISE","act=1,cycl=1,cell=3,stt=1")]
            }
      ]
    , testGroup "Decoding"
      [ testProperty "Decoding decodes properly" (\mc -> decodeMCell (encodeMCell mc) == Right mc)
      ]
    ]

testCase :: TestName -> FilePath -> String -> TestTree
testCase name file text = goldenVsString name ("tests/CA/Format/MCell/"++file++".golden") (pure $ pack text)
