{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

module CA.ALPACA.Parse
    (
      DefnType(..)
    , Name
    , ALPACA(..)
    , Defn(..)
    , StateDefn(..)
    , ClassDefn(..)
    , NbhdDefn(..)
    , Rule(..)
    , StateRef(..)
    , Expression(..)
    , LogOp(..)
    , Term(..)
    , BoolPrim(..)
    , Neighbourhood
    , parseALPACA ) where

import Control.Applicative (empty)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Monoid (First(..))
import Data.Bifunctor (bimap)
import Data.Void (Void)

import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State.Strict as S
import qualified Data.Map.Strict as Map
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import CA
import CA.Utils (Direction(..))

-- A descriptive type synonym
type State = Int

data DefnType = StateType | ClassType | NbhdType
newtype Name (a :: DefnType) = Name { getName :: String } deriving (Show, Eq)
data ALPACA = ALPACA [Defn] (Maybe [[State]])
instance Show ALPACA where
    show (ALPACA defns _) = "ALPACA " ++ show defns ++ " <<initial pattern>>"
data Defn = StateDefn' (StateDefn State) | ClassDefn' ClassDefn | NbhdDefn' NbhdDefn deriving (Show)
-- `StateDefn` is a bit problematic. We want states to be represented by
-- `Finite n` values, but we have no way of determining `n` ahead of time.
-- However, by leaving `intType` polymorphic, we can use the unbounded (for our
-- purposes) `State` type in this module, and then convert it to `Finite n` when
-- we have enough information (i.e. in the CA.ALPACA module). A `Functor`
-- instance is provided for convenience.
data StateDefn intType = StateDefn intType (Name 'StateType) (Maybe Char) [Name 'ClassType] [Rule]
    deriving (Show, Functor)
data ClassDefn = ClassDefn (Name 'ClassType) [Name 'ClassType] [Rule]
    deriving (Show)
data NbhdDefn = NbhdDefn (Name 'NbhdType) Neighbourhood
    deriving (Show)
data Rule = Rule StateRef (Maybe Expression) deriving (Show)
data StateRef = StateID (Name 'StateType)
              | DirRef [Direction]
              | Me
              deriving (Show)
data Expression = ExprLog Term LogOp Expression
                | ExprLeaf Term
                deriving (Show)
data LogOp = And | Or | Xor deriving (Show)
data Term = AdjacencyPred Int (Maybe (Either Neighbourhood (Name 'NbhdType))) (Either StateRef (Name 'ClassType))
          | ExpressionTerm Expression
          | Not Term
          | BoolPrim BoolPrim
          | RelationalPred StateRef (Either StateRef (Name 'ClassType))
          deriving (Show)
data BoolPrim = BPTrue | BPFalse | BPGuess deriving (Show)
type Neighbourhood = [[Direction]]

type Parser = ParsecT Void String (S.State State)

parseALPACA :: String -> Either String (ALPACA, Map.Map Int (String, Maybe Char))
parseALPACA = bimap parseErrorPretty (with names) . flip S.evalState 0 . runParserT alpaca ""
  where
    with :: (a -> b) -> a -> (a, b)
    with f a = (a, f a)

    names :: ALPACA -> Map.Map Int (String, Maybe Char)
    names (ALPACA defns _) = Map.fromList $ mapMaybe go defns
      where
        go (StateDefn' (StateDefn s (Name n) c _ _)) = Just (s, (n, c))
        go _ = Nothing

sc :: Parser ()
sc = L.space space1 empty blockComment
  where
    blockComment :: Parser ()
    blockComment = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

identifier :: Parser String
identifier = lexeme $ (:) <$> letterChar <*> many alphaNumChar

name :: Parser (Name a)
name = Name <$> identifier

alpaca :: Parser ALPACA
alpaca = between sc eof $ do
    defns <- sepBy1 defn (symbol ";")
    e <- (fmap . fmap . fmap . fmap) (getStates defns) end
    return $ ALPACA defns e
  where
    end = Nothing <$  symbol "."
      <|> Just    <$> (symbol "begin" *> initConfig)

    getStates :: [Defn] -> Char -> State
    getStates defns = fromMaybe emptyState . lookupState
      where
        emptyState :: State
        emptyState = let defndStates = mapMaybe (\case { (StateDefn' (StateDefn s _ c _ _)) -> Just (s, c) ; _ -> Nothing }) defns
                         spaceState = firstWith (\case { (s, Just ' ') -> Just s ; _ -> Nothing}) defndStates
                     in  fromMaybe (maybe 0 fst $ headMay defndStates) spaceState
          where
            headMay (x:_) = Just x
            headMay [] = Nothing

        lookupState :: Char -> Maybe State
        lookupState c = flip firstWith defns $ \case
                            (StateDefn' (StateDefn s _ (Just c') _ _)) | c == c' -> Just s
                            _                                                    -> Nothing

        firstWith :: Foldable t => (a -> Maybe b) -> t a -> Maybe b
        firstWith p = getFirst . foldMap (First . p)

initConfig :: Parser [[Char]]
initConfig = lines <$> some anyChar

defn :: Parser Defn
defn = stateDefn <|> classDefn <|> nbhdDefn
  where
    stateDefn = do
        try $ symbol "state"
        state <- lift S.get
        n <- name
        dispChar <- lexeme $ optional ((char '"') *> anyChar <* char ('"'))
        members <- many classRef
        rules <- sepBy rule (symbol ",")
        lift $ S.modify (+1)
        return $ StateDefn' $ StateDefn state n dispChar members rules
    classDefn = do
        try $ symbol "class"
        n <- name
        supers <- many classRef
        rules <- sepBy rule (symbol ",")
        return $ ClassDefn' $ ClassDefn n supers rules
    nbhdDefn = do
        symbol "neighbourhood"
        n <- name
        nbhd <- neighbourhood
        return $ NbhdDefn' $ NbhdDefn n nbhd

classRef :: Parser (Name 'ClassType)
classRef = symbol "is" *> name

stateRef :: Parser StateRef
stateRef = DirRef  <$> arrowChain
       <|> StateID <$> name
       <|> Me      <$  symbol "me"

rule :: Parser Rule
rule = Rule <$> (symbol "to" *> stateRef) <*> optional (symbol "when" *> expression)

expression :: Parser Expression
expression = try (ExprLog <$> term <*> logOp) <*> expression
         <|> ExprLeaf <$> term

logOp :: Parser LogOp
logOp = (And <$ symbol "and")
    <|> (Or  <$ symbol "or")
    <|> (Xor <$ symbol "xor")

term :: Parser Term
term = AdjacencyPred <$> lexeme L.decimal
                     <*> optional (symbol "in" *> eitherP neighbourhood name)
                     <*> eitherP (optional (symbol "=") *> stateRef) classRef
   <|> ExpressionTerm <$> between (symbol "(") (symbol ")") expression
   <|> Not <$> (symbol "not" *> term)
   <|> BoolPrim <$> boolPrim
   <|> RelationalPred <$> stateRef <*> eitherP' (optional (symbol "=") *> stateRef) classRef
 where
   -- This is the same as 'eitherP', but tries the right option before the left
   eitherP' a b = (Right <$> b) <|> (Left <$> a)

boolPrim :: Parser BoolPrim
boolPrim = BPTrue  <$ symbol "true"
       <|> BPFalse <$ symbol "false"
       <|> BPGuess <$ symbol "guess"

neighbourhood :: Parser Neighbourhood
neighbourhood = between (symbol "(") (symbol ")") $ sepBy arrowChain sc

arrowChain :: Parser [Direction]
arrowChain = lexeme $ some direction

direction :: Parser Direction
direction = LeftDir  <$ char '<'
        <|> RightDir <$ char '>'
        <|> UpDir    <$ char '^'
        <|> DownDir  <$ char 'v'
