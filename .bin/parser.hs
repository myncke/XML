{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Parser where

import Data.Char
import Control.Monad
import Control.Monad.State
import Control.Applicative

import ParseData
import Utils
import Prelude hiding (Left, Right)
--------------------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------------------
newtype Parser a = Parser { runParser :: StateT String Maybe a }
  -- runStateT :: StateT s m a -> s -> m (a, s)
  deriving (Functor, Monad, Applicative, Alternative)

-- Return parsed statement, assuming at least one successful parse
parse ::  String -> IO (Maybe Stmt)
parse = parse' >>= \stmt -> return stmt

parse' s = case apply s of
  Just (s,_) -> return (Just s)
  Nothing    -> return Nothing

-- Apply/run a parser
apply :: String -> Maybe (Stmt, String)
apply = runStateT (runParser sequenceOfStmt)

--------------------------------------------------------------------------------
-- Characters
--------------------------------------------------------------------------------
char :: Parser Char
char = Parser $ get >>= \ p -> case p of
  [] -> fail "No more stuff"
  (c:cs) -> put cs >> return c

 -- Parse a character satisfying a predicate (e.g., isDigit)
spot :: (Char -> Bool) -> Parser Char
spot p = do { c <- char; guard (p c); return c }

 -- Match a given character
token :: Char -> Parser Char
token c = spot (== c)

-- Match a given Parse without whitespace
token' :: Parser a -> Parser a
token' p = whitespace *> p <* whitespace

-- Returns a parser that checks for the given predicate
satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = Parser $ do
    inp <- get
    case inp of
      []                 -> fail "No more input"
      c : cs | pred c    -> put cs >> return c
             | otherwise -> fail "Couldn't satisfy"

-- Parses a list of possible chars
oneOf :: String -> Parser Char
oneOf cs = satisfy (`elem` cs)

--------------------------------------------------------------------------------
-- Strings
--------------------------------------------------------------------------------
match :: String -> Parser String
match [] = return []
match (x:xs) = do
  y <- token x;
  ys <- match xs;
  return (y:ys)

newlines :: Parser String
newlines =  many $ token '\n'

whitespace :: Parser String
whitespace = many (oneOf " \t")

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
-- Identifier
--------------------------------------------------------------------------------
identifier :: Parser Identifier
identifier = token' $ (:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)

identifier' :: Parser Identifier
identifier' = do
  token' $ match $ openTag  _ID
  s <-  token' $ (:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)
  token' $ match $ closeTag  _ID
  return s

--------------------------------------------------------------------------------
-- Numbers
--------------------------------------------------------------------------------
-- match a natural number
parseNat :: Parser Int
parseNat = plus (spot isDigit) >>= \s -> return (read s)
-- match a negative number
parseNeg :: Parser Int
parseNeg = do token '-'
              n <- parseNat
              return (-n)
-- match an integer
parseInt :: Parser Int
parseInt = parseNat <|> parseNeg

--------------------------------------------------------------------------------
-- Arithmetic Expressions
--------------------------------------------------------------------------------
parseAExp :: Parser AExp
parseAExp =  parseLit <|> parseAdd <|> parseMul <|> parseDiv <|>
             parseSub <|> parseVar <|> parseMod
  where
    parseAdd = parseAExp' _PLUS >>= \(d, e) -> return (Add d e)
    parseMul = parseAExp' _MUL  >>= \(d, e) -> return (Mul d e)
    parseDiv = parseAExp' _DIV  >>= \(d, e) -> return (Div d e)
    parseSub = parseAExp' _MIN  >>= \(d, e) -> return (Min d e)
    parseMod = parseAExp' _MOD  >>= \(d, e) -> return (Mod d e)
    --
    parseVar =  open _VAR
             >> identifier >>= \i
             -> close _VAR
             >> return (Var i)
    --
    parseLit =  open _VALUE
             >> parseInt >>= \n
             -> close _VALUE
             >> return (ALit n)

parse2AExp :: Parser (AExp, AExp)
parse2AExp =  parseAExp >>= \d
           -> newlines
           >> parseAExp >>= \e
           -> return (d, e)

parseAExp' :: String -> Parser (AExp, AExp)
parseAExp' s =  open s
             >> parse2AExp >>= \(d, e)
             -> close s
             >> return (d, e)


--------------------------------------------------------------------------------
-- Boolean Expressions
--------------------------------------------------------------------------------
parseBExp :: Parser BExp
parseBExp =  parseLit <|> parseNot <|> parseBoolOP
  where
    parseLit  = parseBool >>= \b -> return (BLit b)
    parseNot  = open _NOT >> parseBExp >>= \b -> close _NOT >> return (Not b)

parseBool :: Parser Bool
parseBool = true <|> false
  where
    true  = open _BOOL >> tag _TRUE  >> close _BOOL >> return True
    false = open _BOOL >> tag _FALSE >> close _BOOL >> return False

parseBoolOP :: Parser BExp
parseBoolOP = parseAnd <|> parseOr <|> parseEqualsA
  where
    parseAnd     =  open _AND
                 >> parseBExp >>= \p
                 -> newlines
                 >> parseBExp >>= \q
                 -> close _AND
                 >> return (BBool And p q)
    --
    parseOr      =  open _OR
                 >> parseBExp >>= \p
                 -> newlines
                 >> parseBExp >>= \q
                 -> close _OR
                 >> return (BBool Or p q)
    --
    parseEqualsA =  open _EQUALS
                 >> parse2AExp >>= \(d, e)
                 -> close _EQUALS
                 >> return (ABool Equals d e)

--------------------------------------------------------------------------------
-- Print Expressions
--------------------------------------------------------------------------------
parsePExp :: Parser PExp
parsePExp =  a <|> s <|> b -- <|> q
  where
    a = parseAExp >>= \e -> return (APrint e)
    b = parseBExp >>= \e -> return (BPrint e)
    s = do
      open _STRING
      i <- many $ satisfy (/= '\n')
      close _STRING
      return (SPrint i )

--------------------------------------------------------------------------------
-- Jef Commands
--------------------------------------------------------------------------------
parseJExp :: Parser JefCommand
parseJExp = light <|> go
  where
    light =  open _LIGHT
          >> parse2AExp >>= \(l, r)
          -> newlines
          >> parse2AExp >>= \(g, b)
          -> close _LIGHT
          >> return (SetLight l r g b)
    --
    go    = open _GO
          >> parseDirection >>= \d
          -> close _GO
          >> return (Go d)

parseDirection :: Parser Direction
parseDirection = forward <|> backward <|> left <|> right
  where
    forward  = tag _FORWARD  >> return (Forward)
    backward = tag _BACKWARD >> return (Backward)
    left     = tag _LEFT     >> return (Left)
    right    = tag _RIGHT    >> return (Right)

          -- (\_ d -> Go d) <$> token (char literal_GO) <*> direction)


        --  <|> ((\_ d _ -> Go d)
        --  <$> open _GO

        -- JefCommand = SetLight Light Int Int Int
        --   | Go Direction
        --   | Stop
        --     deriving (Show)


  -- token (char literal_LIGHT) <*> integer <*> integer <*> integer <*> integer)
  -- <|> ((\_ l -> SetLight l 0 0 0) <$> token (char literal_LIGHT_OUT) <*> integer)
  -- <|> ((\_ d -> Go d) <$> token (char literal_GO) <*> direction)
  -- <|> (const Stop <$> token (char literal_STOP))

--------------------------------------------------------------------------------
-- Statements
--------------------------------------------------------------------------------
statement :: Parser Stmt
statement   = commentStmt <|> assignStmt <|> printStmt <|> ifStmt <|>
              blockStmt   <|> whileStmt  <|> jefStmt


-- ASSIGN STATEMENT
assignStmt :: Parser Stmt
assignStmt  =  open _ASSIGN
            >> identifier' >>= \i
            -> newlines
            >> parseAExp   >>= \v
            -> close _ASSIGN
            >> return (Assign i v)

-- PRINT STATEMENT
printStmt :: Parser Stmt
printStmt   =  open _PRINT
            >> parsePExp >>= \s -- many $ satisfy (/= '<') -- Todo: match "</print>"
            -> close _PRINT
            >> return (Print s)

-- JEF STATEMENT
jefStmt :: Parser Stmt
jefStmt     =  open _JEF
            >> parseJExp >>= \c
            -> close _JEF
            >> return (Jef c)

-- IFELSE STATMENT
ifStmt :: Parser Stmt
ifStmt      =  open _IFELSE
            >> sepBy1 caseStmt (whitespace *> some(token '\n') <* whitespace) >>= \s
            -> close _IFELSE
            >> return (If s)
--
caseStmt :: Parser Case
caseStmt    =  open _CASE
            >> parseBExp >>= \b
            -> newlines
            >> blockStmt >>= \s
            -> close _CASE
            >> return (BoolBlock b s)

blockStmt :: Parser Stmt
blockStmt   =  open _BLOCK
            >> sequenceOfStmt >>= \s
            -> close _BLOCK
            >> return s

whileStmt :: Parser Stmt
whileStmt   =  open _WHILE
            >> caseStmt >>= \c
            -> close _WHILE
            >> return (While c)

-- COMMENT STATEMENT
commentStmt :: Parser Stmt
commentStmt =  do
  tag _OPEN_COMMENT
  many $ token '\n'
  many $ satisfy (/= '-') -- Todo: match "-->"
  many $ token '\n'
  tag  _CLOSE_COMMENT
  return Skip

--------------------------------------------------------------------------------
-- Magic
--------------------------------------------------------------------------------
tag :: String -> Parser String
tag s = token' $ match (s)

open :: String -> Parser String
open s = tag (openTag s) >>= \ _ -> many $ token '\n'

close :: String -> Parser String
close s = do { many $ token '\n'; tag (closeTag s) }

sepBy :: Parser a -> Parser s -> Parser [a]
sepBy p s = liftA2 (:) p ((s *> sepBy1 p s) <|> pure []) <|> pure []

sepBy1 :: Parser a -> Parser s -> Parser [a]
sepBy1 p s = scan where scan = liftA2 (:) p $ (s *> scan) <|> pure []

sequenceOfStmt :: Parser Stmt
sequenceOfStmt = Seq <$> sepBy1 statement (whitespace *> some(token '\n') <* whitespace)
