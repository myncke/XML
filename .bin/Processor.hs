module Processor where

import Parser
import ParseData
import Utils
import MBot
import System.HIDAPI

import Control.Monad.State

--------------------------------------------------------------------------------
-- Evaluator
--------------------------------------------------------------------------------
-- to do
process :: Stmt -> IO ()
process program =
  openMBot >>= \d
  -> runStateT (evaluate d program) []
  >> closeMBot d
  >> return ()

type Evaluator = StateT Environment IO

--------------------------------------------------------------------------------
-- Environment (Map of Strings and Ints)
--------------------------------------------------------------------------------
type Environment = [(Identifier, Int)]
type EnvironmentT = (Environment, Device) -- todo lenses

find :: Identifier -> Environment -> Maybe Int
find i [] = Nothing
find i ((k,v):e)
  | i == k = Just v
  | otherwise = lookup i e

add :: Identifier -> Int -> Environment -> Environment
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

evalAExp :: AExp -> Evaluator Int
evalAExp (ALit n)  = return n
evalAExp (Add e f) = evalAExp e >>= \a -> evalAExp f >>= \b -> return (a + b)
evalAExp (Mul e f) = evalAExp e >>= \a -> evalAExp f >>= \b -> return (a * b)
evalAExp (Div e f) = evalAExp e >>= \a -> evalAExp f >>= \b -> return (div a b)
evalAExp (Min e f) = evalAExp e >>= \a -> evalAExp f >>= \b -> return (a - b)
evalAExp (Mod e f) = evalAExp e >>= \a -> evalAExp f >>= \b -> return (mod a b)
evalAExp (Var  n)  = get >>= \env -> case find n env of
  Just b -> return b
  Nothing -> fail "Error: couldn't find variable"

--------------------------------------------------------------------------------
-- Boolean Expressions
--------------------------------------------------------------------------------

evalBExp :: BExp -> Evaluator Bool
evalBExp (BLit b) = return b
evalBExp (Not n)  = evalBExp n >>= \ b -> return (not b)
evalBExp (ABool o d e) = do
  o' <- evalABoolOp  o
  d' <- evalAExp d
  e' <- evalAExp e
  return (o' d' e')
evalBExp (BBool o p q) = do
  p' <- evalBExp p
  q' <- evalBExp q
  o' <- evalBBoolOp  o
  return (o' p' q')

evalBBoolOp :: BOp ->  Evaluator (Bool -> Bool -> Bool)
evalBBoolOp And     = return (&&)
evalBBoolOp Or      = return (||)

evalABoolOp :: BOp ->  Evaluator (Int -> Int -> Bool)
evalABoolOp Equals  = return (==)
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
-- evalCase :: Case -> Evaluator ()
-- evalCase (BoolBlock b s) = evalBExp b >>= \b' -> if b' then evaluate d s else return ()

--------------------------------------------------------------------------------
-- Statements
--------------------------------------------------------------------------------
-- evaluate :: Stmt -> StateT Environment IO ()
--  where StateT s m a:
-- s = state = Environment = [(Identifier, Int)]
-- m = monad = IO
-- a = type  = () -- empty
evaluate :: Device -> Stmt -> Evaluator ()
evaluate d (Seq [])     = return ()
evaluate d (Seq (x:xs)) = (evaluate d x) >>= \_ -> evaluate d (Seq xs)
evaluate d (Assign i e) = (evalAExp e) >>= \v -> get >>= (put . (add i v))
evaluate d (Print s)    = evalPExp s
evaluate d (If [])      = return ()
evaluate d (If ((BoolBlock b s):xs) )
                      =  evalBExp b >>= \b'
                      -> if b' then evaluate d s else evaluate d (If xs)
evaluate d (While (BoolBlock b s))
                      =  evalBExp b >>= \b'
                      -> if b'
                         then evaluate d s >> evaluate d (While (BoolBlock b s))
                         else return ()
evaluate d (Jef c)      = evalJefCommand d c
evaluate d Skip         = return ()



-- if (cond) (do x) = Seq (a=true) (While (cond && a==true) (Seq (do x) (a=false))
--------------------------------------------------------------------------------
-- MBot Stuff
--------------------------------------------------------------------------------
evalJefCommand :: Device -> JefCommand -> Evaluator ()
evalJefCommand d (SetLight l r g b) =
     evalAExp l >>= \l'
  -> evalAExp r >>= \r'
  -> evalAExp g >>= \g'
  -> evalAExp b >>= \b'
  -> lift $ do sendCommand d $ setRGB l' r' g' b'
  -- -> return ()


    -- sendCommand d $ setRGB (round l') (round r') (round g') (round b')
    -- closeMBot d
