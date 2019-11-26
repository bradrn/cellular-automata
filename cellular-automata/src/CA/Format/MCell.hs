{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns     #-}

{-|
Module     : CA.Format.MCell
Copyright  : (c) Brad Neimann 2017-2018
License    : MIT
Maintainer : brad.neimann@hotmail.com

Encoding and decoding the [MCell file format](http://psoup.math.wisc.edu/mcell/ca_files_formats.html#MCell).
-}
module CA.Format.MCell where

import Control.Arrow ((>>>), (&&&))
import Control.Monad ((<=<), join)
import Data.Bifunctor (bimap)
import Data.Char (isUpper, isLower, ord, isSpace, chr)
import Data.List (groupBy, group, intercalate)
import Data.Maybe (fromMaybe, mapMaybe, catMaybes, maybeToList)

import Data.List.Split (splitOn)
import Safe (readMay, toEnumMay)

import qualified CA.Format.Combinators as P
import CA.Types
import CA.Universe (toList, fromList)

-- * Types

-- | A record containing all elements of the MCell format. For details on what
-- each field means see <http://psoup.math.wisc.edu/mcell/ca_files_formats.html#MCell>
data MCell = MCell { game :: Maybe Game
                   , rule :: Maybe String
                   , speed :: Maybe Int
                   , ccolors :: Maybe Int
                   , coloring :: Maybe Coloring
                   , wrap :: Maybe Bool
                   , palette :: Maybe String
                   , description :: Maybe String
                   , universe :: Universe Int
                   , diversities :: [(String, String)]
                   } deriving (Show)

-- | A convenient constructor for an 'MCell' value. Since there are only two
-- non-'Maybe' fields in 'MCell', this constructor can be used to set all other
-- fields to 'Nothing', after which other fields can be modified as desired
-- using record update syntax. Usage is something like e.g.
-- @('mcell' myUn myDiv){ 'game' = 'Generations' }@.
mcell :: Universe Int -> [(String, String)] -> MCell
mcell = MCell Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data Game = Life
          | Generations
          | WeightedLife
          | VoteForLife
          | RulesTable
          | CyclicCA
          | OneDTotalistic
          | NeumannBinary
          | GeneralBinary
          | LargerThanLife
          | Margolus
          | UserDLL
          | SpecialRules
          deriving (Bounded, Enum)

instance Show Game where
    show Life           = "Life"
    show Generations    = "Generations"
    show WeightedLife   = "Weighted Life"
    show VoteForLife    = "Vote for Life"
    show RulesTable     = "Rules table"
    show CyclicCA       = "Cyclic CA"
    show OneDTotalistic = "1-D totalistic"
    show NeumannBinary  = "Neumann binary"
    show GeneralBinary  = "General binary"
    show LargerThanLife = "Larger than Life"
    show Margolus       = "Margolus"
    show UserDLL        = "User DLL"
    show SpecialRules   = "Special rules"

data Coloring = Standard | Alternate deriving (Bounded, Enum, Show)

-- * Encoding and decoding

encodeMCell :: MCell -> String
encodeMCell MCell{..} = concat $ catMaybes
    [ Just "#MCell 4.00\n"
    , (tagged "GAME"     . show)                    <$> game
    ,  tagged "RULE"                                <$> rule
    , (tagged "SPEED"    . show)                    <$> speed
    , Just $ tagged "BOARD" $ showBoard $ size universe
    , (tagged "CCOLORS"  . show)                    <$> ccolors
    , (tagged "COLORING" . showEnum True)           <$> coloring
    , (tagged "WRAP"     . showEnum False)          <$> wrap
    ,  tagged "PALETTE"                             <$> palette
    ,  tagged "D"                                   <$> description
    , Just $ tagged "L" $ toRLE $ universe
    -- We use `intercalate "\n"` and not `unlines` to avoid an extra newline at the end
    , (tagged "DIV" . intercalate "\n" . fmap showDiversity) <$> maybeNull diversities
    ]
  where
    tagged s str = if null str
                   then '#':s ++ "\n"
                   -- We use `splitOn "\n"` instead of `lines` to preserve any
                   -- newlines which happen to be at the very end
                   else concatMap (\l -> ('#':s) ++ (' ':l) ++ "\n") $ splitOn "\n" str

    -- This is basically equivalent to Data.List.NonEmpty.nonEmpty but we can't
    -- use that because unlines doesn't work on NonEmpty lists
    maybeNull :: [a] -> Maybe [a]
    maybeNull [] = Nothing
    maybeNull xs = Just xs

    showBoard (Coord x, Coord y) = show x ++ "x" ++ show y
    showDiversity (k, v) = ('#':k) ++ (',':v)

    showEnum :: Enum e => Bool -> e -> String
    showEnum addOne = show . (if addOne then (+1) else id) . fromEnum

    toRLE :: Universe Int -> String
    toRLE = intercalate "$" . fmap processRow  . toList
      where
        processRow = concatMap (\(r, v) -> (if r == 1 then "" else show r) ++ toChars v) . runEncode . stripDead

        runEncode :: Eq a => [a] -> [(Int, a)]
        runEncode = fmap (length &&& head) . group

        stripDead = reverse . dropWhile (==0) . reverse

        toChars 0 = "."
        toChars n
            | (0 < n) && (n <= 24) = pure $ chr $ 64 + n -- maps numbers 1..24 to chars 'A'..'X'
            | 24 < n =
                let (prefix, suffix) = n `quotRem` 24
                    -- The if statement is required to make the sequence go
                    -- '46=aV, 47=aW, 48=aX, 49=bA' instead of '46=aW, 47=aX, 48=bB, 49=bC'
                    -- Basically, avoiding an off-by-one error, except the
                    -- modulus operation makes it a bit trickier
                    prefix' = chr $ 96 + (if suffix == 0 then prefix-1 else prefix)
                    suffix' = chr $ 65 + (if suffix == 0 then 24       else suffix-1)
                in prefix':suffix':[]
            | otherwise = error "ERROR: Negative integer in universe!"

decodeMCell :: String -> Either String MCell
decodeMCell str = do
    let tagged = getTags $ lines str
    game        <- extractTag tagged "GAME"     readGame
    rule        <- extractTag tagged "RULE"     pure
    speed       <- extractTag tagged "SPEED"    (readMay @Int)
    boardSize   <- extractTag tagged "BOARD"    parseBoard
    ccolors     <- extractTag tagged "CCOLORS"  (readMay @Int)
    coloring    <- extractTag tagged "COLORING" ((toEnumMay @Coloring . (subtract 1)) <=< readMay @Int)
    wrap        <- extractTag tagged "WRAP"     ( toEnumMay @Bool                     <=< readMay @Int)
    palette     <- extractTag tagged "PALETTE"  pure
    description <- extractTag tagged "D"        pure
    universe <- join $ fmap (maybeToEither "Could not find mandatory #L tag") $
        extractTag tagged "L" (parseUniverse boardSize . filter (not.isSpace))
    diversities <- (concat . maybeToList) <$> extractTag tagged "DIV" (pure . parseDiversities)
    return MCell{..}
  where
    getTags :: [String] -> [(String, String)]
    getTags = groupBy (\x y -> takeWhile (not.isSpace) x == takeWhile (not.isSpace) y)
          >>> mapMaybe unTag

    extractTag :: [(String, v)] -> String -> (v -> Maybe a) -> Either String (Maybe a)
    extractTag tagged tag op = sequenceA $ (maybeToEither $ "Could not parse #" ++ tag ++ " tag") . op <$> lookup tag tagged

    maybeToEither :: e -> Maybe a -> Either e a
    maybeToEither e Nothing = Left e
    maybeToEither _ (Just a) = Right a

    unTag :: [String] -> Maybe (String, String)
    unTag xs@(x:_) =
        case takeWhile (/=' ') x of
            -- We use `intercalate "\n"` and not `unlines` to avoid an extra newline at the end
            '#':tag -> Just (tag, intercalate "\n" $ drop (length tag + 2) <$> xs)
            _ -> Nothing
    unTag _ = Nothing

    padEdge :: Maybe Int -> [[Int]] -> [[Int]]
    padEdge len l =
        let maxlen = fromMaybe (foldr max 0 $ fmap length l) len
        in fmap (\m -> take maxlen $ m ++ repeat 0) l

    parseBoard :: String -> Maybe (Int, Int)
    parseBoard = P.runParser' $ (,) <$> P.decimal <* P.char 'x' <*> P.decimal

    parseUniverse :: Maybe (Int, Int) -> String -> Maybe (Universe Int)
    parseUniverse (fmap fst -> height) u =
        let rows = splitOn "$" u
            parsed = traverse parseRow rows
        in (fromList . padEdge height) <$> parsed
      where
        parseRow :: String -> Maybe [Int]
        parseRow = (fmap concat .) $ P.runParser' $
            P.many ((replicate <$> P.decimal <*> parseState) P.<|> fmap pure parseState)

        parseState :: P.Parser Int
        parseState = (0 <$ P.char '.')
               P.<|> (decodeState <$> P.optional (P.select isLower) <*> (P.select isUpper))
          where
            decodeState p s = maybe 0 ((24*).(subtract 96).ord) p + (ord s - 64)

    parseDiversities :: String -> [(String, String)]
    parseDiversities = fmap (bimap (drop 1) (drop 1) . break (==',') . dropWhile isSpace) . lines

    readGame "Life"             = Just Life
    readGame "Generations"      = Just Generations
    readGame "Weighted Life"    = Just WeightedLife
    readGame "Vote for Life"    = Just VoteForLife
    readGame "Rules table"      = Just RulesTable
    readGame "Cyclic CA"        = Just CyclicCA
    readGame "1-D totalistic"   = Just OneDTotalistic
    readGame "Neumann binary"   = Just NeumannBinary
    readGame "General binary"   = Just GeneralBinary
    readGame "Larger than Life" = Just LargerThanLife
    readGame "Margolus"         = Just Margolus
    readGame "User DLL"         = Just UserDLL
    readGame "Special rules"    = Just SpecialRules
    readGame _                  = Nothing
