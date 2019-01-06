module CA.ALPACA.Stylesheets
    ( Stylesheet
    , Ruleset
    , Selector(..)
    , Decl(..)
    , Color(..)
    , parseStylesheet ) where

import Data.Bifunctor (first)
import Data.Void (Void)
import Numeric (readHex)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Stylesheet = [(Selector, Ruleset)]
type Ruleset = [Decl]
data Selector = Class String deriving (Show, Eq)
data Decl = Fill Color deriving (Show, Eq)
data Color = RGB Double Double Double deriving (Show, Eq)

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

identifier :: Parser String
identifier = lexeme $ (:) <$> letterChar <*> many alphaNumChar

parseStylesheet :: String -> Either String Stylesheet
parseStylesheet = first parseErrorPretty . runParser stylesheet ""

stylesheet :: Parser Stylesheet
stylesheet = many ruleset

ruleset :: Parser (Selector, Ruleset)
ruleset = (,) <$> selector <*> between (symbol "{") (symbol "}") (many $ decl <* symbol ";")

selector :: Parser Selector
selector = Class <$> (char '.' *> identifier)

decl :: Parser Decl
decl = Fill <$> (symbol "fill:" *> color)

color :: Parser Color
color = process <$> (char '#' *> hexNum) <*> hexNum <*> hexNum
  where
    hexNum :: Parser Double
    hexNum = (\x y -> fst $ head $ readHex (x:y:[])) <$> hexDigitChar <*> hexDigitChar

    process :: Double -> Double -> Double -> Color
    process r g b = RGB (r/255) (g/255) (b/255)
