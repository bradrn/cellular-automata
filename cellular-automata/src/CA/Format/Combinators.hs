{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase    #-}

{-|
Module     : CA.Format.Combinators
Copyright  : (c) Brad Neimann 2017-2018
License    : MIT
Maintainer : brad.neimann@hotmail.com

A /very/ simple implementation of a set of parser combinators.
Included to avoid incurring a dependency on Parsec or Megaparsec.

Note that this implementation has a well-known space leak; however,
for such a simple use case this probably doesn't matter.
|-}
module CA.Format.Combinators (module Control.Applicative, module CA.Format.Combinators) where

import Control.Applicative (Alternative(..), liftA2)
import Data.Char (isDigit)

import Safe (readMay)

-- | Represents a parser of 'String's to @a@s, with failure.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }
    deriving (Functor)

runParser' :: Parser a -> (String -> Maybe a)
runParser' (Parser p) = \i -> fst <$> p i

instance Applicative Parser where
    pure a = Parser $ \i -> Just (a, i)
    liftA2 f (Parser pa) (Parser pb) = Parser $ \i ->
        case (pa i) of
            Nothing -> Nothing
            Just (a, i') ->
                case (pb i') of
                    Nothing -> Nothing
                    Just (b, i'') -> Just (f a b, i'')

-- | __WARNING:__ '<|>' has a space leak. Avoid usage with large inputs!
instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser p) <|> (Parser q) = Parser $ \i ->
        case p i of
            Nothing -> q i
            success -> success

instance Monad Parser where
    return = pure
    (Parser pa) >>= apb = Parser $ \i ->
        case (pa i) of
            Nothing -> Nothing
            Just (a, i') -> runParser (apb a) i'

-- | __WARNING:__ This has a space leak. Avoid usage with large inputs!
optional :: Parser a -> Parser (Maybe a)
optional (Parser p) = Parser $ \i ->
    case (p i) of
        Nothing -> Just (Nothing, i)
        Just (a, rest) -> Just (Just a, rest)

-- | \'Flatten' a parser of 'Maybe' into a normal parser. The output
-- fails if the input parser gives 'Nothing'.
flatten :: Parser (Maybe a) -> Parser a
flatten (Parser p) = Parser $ \i ->
    case p i of
        Just (Just a, rest) -> Just (a, rest)
        _ -> Nothing

(>>$) :: Parser a -> (a -> Maybe b) -> Parser b
p >>$ f = flatten (f <$> p)

-- | Parse a sequence of characters given a predicate. Does not fail;
-- if no characters satisfy the predicate, returns the empty string.
spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ Just . span f

-- | Parse a sequence of characters given a predicate. Fails if no
-- characters satisfy the predicate.
spanP' :: (Char -> Bool) -> Parser String
spanP' f = Parser $ \i ->
    case (span f i) of
        ([], _) -> Nothing
        (o, rest) -> Just (o, rest)

-- | Fails if there is still more left to parse.
eof :: Parser ()
eof = Parser $ \i -> case i of
    "" -> Just ((), "")
    _  -> Nothing

-- | Parse a specific character.
char :: Char -> Parser ()
char c = Parser $ \case
    (x:xs) | x == c    -> Just ((), xs)
           | otherwise -> Nothing
    [] -> Nothing

-- | Parse one character which satisfies a condition.
select :: (Char -> Bool) -> Parser Char
select p = Parser $ \case
    (x:xs) | p x ->       Just (x, xs)
           | otherwise -> Nothing
    [] -> Nothing

-- | Parses an integer made of multiple digits.
decimal :: Parser Int
decimal = spanP' isDigit >>$ readMay
