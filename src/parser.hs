{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Parser where

import Data.Char
import Data.List
import Control.Monad
import Control.Monad.State
import Control.Applicative

import ParseData
import MBot

--------------------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------------------
newtype Parser a = Parser (String -> [(a, String)])

instance Monad Parser where
  return x = Parser (\s -> [(x,s)])
  m >>= k  = Parser (\s -> [(y, u) | (x, t) <- apply m s,
                                     (y, u) <- apply (k x) t ])

instance MonadPlus Parser where
  mzero = Parser (\s -> [])
  mplus m n = Parser (\s -> apply m s ++ apply n s)
--
instance Functor (Parser) where
  fmap = liftM
--
instance Applicative (Parser) where
  pure = return
  (<*>) = ap
--
instance Alternative (Parser) where
  empty = mzero
  (<|>) = mplus

-- Apply a parser
apply :: Parser a -> String -> [(a, String)]
apply (Parser f) s = f s

-- Return parsed value, assuming at least one successful parse
parse :: Parser a -> String -> a
parse m s = one [ x | (x,t) <- apply m s, t == "" ]
  where
    one []  = error "no parse"
    one [x] = x
    one xs | length xs > 1 = error "ambiguous parse"

--------------------------------------------------------------------------------
-- Characters (parse one char)
--------------------------------------------------------------------------------
char :: Parser Char
char = Parser f
  where
    f [] = []
    f (c:s) = [(c,s)]
 -- Parse a character satisfying a predicate (e.g., isDigit)
spot :: (Char -> Bool) -> Parser Char
spot p = do { c <- char; guard (p c); return c }
 -- Match a given character
token :: Char -> Parser Char
token c = spot (== c)

--------------------------------------------------------------------------------
-- Strings
--------------------------------------------------------------------------------
match :: String -> Parser String
match [] = return []
match (x:xs) = do
  y <- token x;
  ys <- match xs;
  return (y:ys)

match' :: String -> Parser String
match' xs = sequence (map token xs)

whitespace :: String -> String
whitespace x = [c | c <- x , c /= ' ', c /= '\t']

--------------------------------------------------------------------------------
-- Comments
--------------------------------------------------------------------------------

comment :: String -> String
comment "" = ""
comment y@(x:xs)
  | x == '#' = ""
  | otherwise = y

strip :: String -> String
strip = comment . whitespace

--------------------------------------------------------------------------------
-- Sequence
--------------------------------------------------------------------------------
star :: Parser a -> Parser [a]
star p = plus p <|> return []
-- match one or more occurrences
plus :: Parser a -> Parser [a]
plus p = do
  x <- p
  xs <- star p
  return (x:xs)

--------------------------------------------------------------------------------
-- Numbers
--------------------------------------------------------------------------------
-- match a natural number
parseNat :: Parser Float
parseNat = do s <- plus (spot isDigit)
              return (read s)
-- match a negative number
parseNeg :: Parser Float
parseNeg = do token '-'
              n <- parseNat
              return (-n)
-- match an integer
parseInt :: Parser Float
parseInt = parseNat <|> parseNeg

--------------------------------------------------------------------------------
-- Arithmetic Expressions
--------------------------------------------------------------------------------
parseAExp :: Parser AExp
parseAExp = parseLit
         <|> parseAdd
         <|> parseMul
         <|> parseDev
         <|> parseSub
    where
      parseLit = parseInt >>= \n -> return (ALit n)
      --
      parseAdd = do { token '(';
        d <- parseAExp;
        token '+';
        e <- parseAExp;
        token ')';
        return (Add d e) }
      --
      parseMul = do { token '(';
        d <- parseAExp;
        token '*';
        e <- parseAExp;
        token ')';
        return (Mul d e) }
      --
      parseDev = do { token '(';
        d <- parseAExp;
        token '/';
        e <- parseAExp;
        token ')';
        return (Div d e) }
      --
      parseSub = do { token '(';
        d <- parseAExp;
        token '-';
        e <- parseAExp;
        token ')';
        return (Min d e) }


--------------------------------------------------------------------------------
-- Boolean Expressions
--------------------------------------------------------------------------------





--------------------------------------------------------------------------------
-- Statements
--------------------------------------------------------------------------------
statement :: Parser Stmt
statement = undefined

comment' :: Parser Stmt
comment' = undefined
-- comment' = do
--   token $ char literal_COMMENT
--   many $ satisfy (/= '\n')
--   return Skip
