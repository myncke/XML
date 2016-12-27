module Processor where

import Parser
import ParseData

process :: [String] -> IO()
process [] = return ()
process (x:xs) = do
  putStrLn $ show $ evalExp $ parse parseExp x
  process xs

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
-- --
-- type Name = String
--
-- data Term = Var Name
--           | Con Integer
--           | Add Term Term
--           | Lam Name Term
--           | App Term Term
--  deriving (Show,Eq)
--
-- data Value = Wrong
--            | Num Integer
--            | Fun (Value->Value)
--
-- instance Show Value where
--           show Wrong = "Wrong"
--           show (Num i) = "Int " ++ show i
--           show (Fun _) = "function "
--
--
-- type Env = [(Name,Value)]
-- getVar :: Env -> Name -> Value
-- getVar [] _ = Wrong
-- getVar ((k1,v) : r) k2
--  | k1 == k2 = v
--  | otherwise = getVar r k2


--------------------------------------------------------------------------------
-- Other bullshit
--------------------------------------------------------------------------------


-- add :: Value -> Value -> Value
-- add (Num x) (Num y) = Num (x + y)
-- add _ _ = Wrong
-- apply :: Value -> Value -> Value
-- apply (Fun f) t2 = f t2
-- apply _ _ = Wrong
--
-- eval :: Term -> Env -> Value
-- eval (Var x) env = getVar env x
-- eval (Con x) _ = Num x
-- eval (Add t1 t2) env = add (eval t1 env) (eval t2 env)
-- eval (Lam n b ) env = Fun (\x -> eval b ((n,x) : env))
-- eval (App t1 t2) env = apply (eval t1 env) (eval t2 env)
