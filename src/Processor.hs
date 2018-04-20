module Processor where

import Parser
import ParseData
import Utils
import MBot
import System.HIDAPI
import Prelude hiding (Left, Right)


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
-- Statements
--------------------------------------------------------------------------------
-- evaluate :: Stmt -> StateT Environment IO ()
--  where StateT s m a:
-- s = state = Environment = [(Identifier, Int)]
-- m = monad = IO
-- a = type  = () -- empty
evaluate :: Device -> Stmt -> Evaluator ()
evaluate d (If ((BoolBlock b@(BLine Equals Follow q) s):xs) )
                      =  evaluateLineEqual d b  >>= \b'
                      -> if b' then evaluate d s else evaluate d (If xs)
evaluate d (Seq [])     = return ()
evaluate d (Seq (x:xs)) = (evaluate d x) >>= \_ -> evaluate d (Seq xs)
evaluate d (Assign i e) = (evalAExp' d e) >>= \v -> get >>= (put . (add i v))
evaluate d (Print s)    = evalPExp d s
evaluate d (If [])      = return ()
evaluate d (If ((BoolBlock b s):xs) )
                      =  evalBExp d b >>= \b'
                      -> if b' then evaluate d s else evaluate d (If xs)
evaluate d (While (BoolBlock b s))
                      =  evalBExp d b >>= \b'
                      -> if b'
                         then evaluate d s >> evaluate d (While (BoolBlock b s))
                         else return ()
evaluate d (Jef c)      = evalJefCommand d c
evaluate d Skip         = return ()


--------------------------------------------------------------------------------
-- Arithmetic Expressions
--------------------------------------------------------------------------------

evalAExp' :: Device -> AExp -> Evaluator Int
evalAExp' d (JefSensor) = lift $ do
    dist <- readUltraSonic d
    return (floor dist)
evalAExp' d a = evalAExp a


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

evalBExp :: Device -> BExp -> Evaluator Bool
evalBExp d (BLit b) = return b
evalBExp d (Not n)  = evalBExp d n >>= \ b -> return (not b)
evalBExp dev (ABool o d e) = do
  o' <- evalABoolOp  o
  d' <- evalAExp' dev d
  e' <- evalAExp' dev e
  return (o' d' e')
evalBExp d (BBool o p q) = do
  o' <- evalBBoolOp  o
  p' <- evalBExp d p
  q' <- evalBExp d q
  return (o' p' q')

evalBBoolOp :: BOp ->  Evaluator (Bool -> Bool -> Bool)
evalBBoolOp And     = return (&&)
evalBBoolOp Or      = return (||)

evalABoolOp :: BOp ->  Evaluator (Int -> Int -> Bool)
evalABoolOp Equals  = return (==)
evalABoolOp Greater = return (>)
evalABoolOp Lesser  = return (<)

evaluateLineEqual :: Device -> BExp -> Evaluator Bool
evaluateLineEqual d (BLine Equals p q) = do
  r <- evalJefLine d p
  s <- evalJefLine d q
  return (r == s)

--------------------------------------------------------------------------------
-- Print Expressions
--------------------------------------------------------------------------------

evalPExp :: Device -> PExp -> Evaluator ()
evalPExp d (APrint a) = do
  f <- evalAExp' d a
  liftIO (putStrLn $ show f)
  return ()
evalPExp d (SPrint s) = do
  liftIO (putStrLn s)
  return ()
evalPExp d (BPrint b) = do
  f <- evalBExp d b
  liftIO (putStrLn $ show f)
  return ()


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

evalJefCommand d (Go Forward)  = lift $ goAhead d
evalJefCommand d (Go Backward) = lift $ goBackwards d
evalJefCommand d (Go Left)     = lift $ goLeft d
evalJefCommand d (Go Right)    = lift $ goRight d
evalJefCommand d Stop          = lift $ stop d

evalJefLine :: Device -> JefLine -> Evaluator Line
evalJefLine d (JLine l) = return l
evalJefLine d (Follow) = lift $ readLineFollower d >>= \line -> return line
