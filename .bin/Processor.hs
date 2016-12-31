module Processor where

import Parser
import ParseData
import Utils

import Control.Monad.State

--------------------------------------------------------------------------------
-- Evaluator
--------------------------------------------------------------------------------
process :: [String] -> IO()
process = undefined
-- process [] = return ()
-- process (x:xs)
--   | (comment x) == "" = process xs
--   | (println x) /= "" = do putStrLn $ tail x
--                            process xs
--   | otherwise = do putStrLn $ show $ evalAExp $ parse parseAExp $ whitespace  x
--                    process xs

type Evaluator = StateT Environment IO

--------------------------------------------------------------------------------
-- Environment (Map of Strings and Floats)
--------------------------------------------------------------------------------
type Environment = [(Identifier, Float)]

find :: Identifier -> Environment -> Maybe Float
find i [] = Nothing
find i ((k,v):e)
  | i == k = Just v
  | otherwise = lookup i e

add :: Identifier -> Float -> Environment -> Environment
add i f [] = [(i,f)]
add i f ((k,v):e)
  | i == k = (k,f):e
  | otherwise = (k,v):add i f e

remove :: Identifier -> Environment -> Environment
remove i [] = []
remove i ((k,v):e)
  | i == k = e
  | otherwise = (k,v):remove i e

--------------------------------------------------------------------------------
-- Arithmetic Expressions
--------------------------------------------------------------------------------

evalAExp :: AExp -> Evaluator Float
evalAExp (ALit n)  = return n
evalAExp (Add e f) = evalAExp e >>= \ a -> evalAExp f >>= \ b -> return (a + b)
evalAExp (Mul e f) = evalAExp e >>= \ a -> evalAExp f >>= \ b -> return (a * b)
evalAExp (Div e f) = evalAExp e >>= \ a -> evalAExp f >>= \ b -> return (a / b)
evalAExp (Min e f) = evalAExp e >>= \ a -> evalAExp f >>= \ b -> return (a - b)

--------------------------------------------------------------------------------
-- Boolean Expressions
--------------------------------------------------------------------------------

evalBExp :: BExp -> IO Bool
evalBExp (BLit b) = return b
evalBExp (Not n)  = evalBExp n >>= \ b -> return (not b)

--------------------------------------------------------------------------------
-- Statements
--------------------------------------------------------------------------------
evaluate :: Stmt -> Evaluator ()
evaluate (Seq [])     = return ()
evaluate (Seq (x:xs)) = (evaluate x) >>= \_ -> evaluate (Seq xs)
evaluate (Assign i e) = (evalAExp e) >>= \v -> get >>= (put . (add i v))


--------------------------------------------------------------------------------
-- MBot Stuff
--------------------------------------------------------------------------------
