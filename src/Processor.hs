module Processor where

import Parser
import ParseData
import Utils

import Control.Monad.State

--------------------------------------------------------------------------------
-- Evaluator
--------------------------------------------------------------------------------
-- to do
process :: Stmt -> IO()
process program = do
    putStrLn "Processing..."
    runStateT (evaluate program) []
    putStrLn "Done processing!"
    return ()

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
evalAExp (Var  n)  = get >>= \env -> case find n env of
  Just b -> return b
  Nothing -> fail "Error: couldn't find variable"

--------------------------------------------------------------------------------
-- Boolean Expressions
--------------------------------------------------------------------------------

evalBExp :: BExp -> Evaluator Bool
evalBExp (BLit b) = return b
evalBExp (Not n)  = evalBExp n >>= \ b -> return (not b)
evalBExp (ABool o p q) = undefined
evalBExp (BBool o p q) = do
  p' <- evalBExp p
  q' <- evalBExp q
  o' <- evalBOp  o
  return (o' p' q')

evalBOp :: BOp ->  Evaluator (Bool -> Bool -> Bool)
evalBOp And     = return (&&)
evalBOp Or      = return (||)
-- evalBOp Greater = (>)
-- evalBOp Less    = (<)

  -- And
  --   | Or
  --   | Greater
  --   | Less
--------------------------------------------------------------------------------
-- Print Expressions
--------------------------------------------------------------------------------

evalPExp :: PExp -> Evaluator ()
evalPExp (APrint a) = do
  f <- evalAExp a
  liftIO (putStrLn $ show f)
  return ()
evalPExp (SPrint s) = do
  liftIO (putStrLn s)
  return ()
evalPExp (BPrint b) = do
  f <- evalBExp b
  liftIO (putStrLn $ show f)
  return ()

--------------------------------------------------------------------------------
-- Case Block
--------------------------------------------------------------------------------
evalCase :: Case -> Evaluator ()
evalCase (BoolBlock b s) = evalBExp b >>= \b' -> if b' then evaluate s else return ()

--------------------------------------------------------------------------------
-- Statements
--------------------------------------------------------------------------------
-- evaluate :: Stmt -> StateT Environment IO ()
--  where StateT s m a:
-- s = state = Environment = [(Identifier, Float)]
-- m = monad = IO
-- a = type  = () -- empty
evaluate :: Stmt -> Evaluator ()
evaluate (Seq [])     = return ()
evaluate (Seq (x:xs)) = (evaluate x) >>= \_ -> evaluate (Seq xs)
evaluate (Assign i e) = (evalAExp e) >>= \v -> get >>= (put . (add i v))
evaluate (Print s)    = evalPExp s
evaluate (If [])      = return ()
evaluate (If ((BoolBlock b s):xs) )
                      =  evalBExp b >>= \b'
                      -> if b' then evaluate s else evaluate (If xs)
evaluate (While (BoolBlock b s))
                      =  evalBExp b >>= \b'
                      -> if b'
                         then evaluate s >> evaluate (While (BoolBlock b s))
                         else return ()
evaluate Skip         = return ()
evaluate _            = return ()



-- if (cond) (do x) = Seq (a=true) (While (cond && a==true) (Seq (do x) (a=false))
--------------------------------------------------------------------------------
-- MBot Stuff
--------------------------------------------------------------------------------
