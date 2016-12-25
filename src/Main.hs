module Main where

import System.Environment

-- import Control.Monad

-- import Data.Maybe
-- import Data.Either
-- import Data.Map (Map, fromList)



main = do
    args <- getArgs
    case args of
      [file] -> do
        x <- readFile file
        putStr x
      _ -> putStrLn "Wrong number of arguments"


--------------------------------------------------------------------------------
-- Comment
--------------------------------------------------------------------------------

type Name = String

data Term = Var Name
          | Con Integer
          | Add Term Term
          | Lam Name Term
          | App Term Term
 deriving (Show,Eq)

data Value = Wrong
           | Num Integer
           | Fun (Value->Value)

instance Show Value where
          show Wrong = "Wrong"
          show (Num i) = "Int " ++ show i
          show (Fun _) = "function "








































--
-- main  :: IO ()
-- main  = do putStrLn welcome
--            args <- getArgs
--            print args
--            main' args
--
--
-- main'	:: [String] -> IO ()
-- main' args = do content <- readFile (args !! 0)
--                 print . map read . words $ contents
--
--
-- welcome = "  Really Usefull Binary Yodeler \n"++
--           " ===============================\n"

--
-- main = do
--     line <- getLine
--     if null line
--         then return ()
--         else do
--             putStrLn $ reverseWords line
--             main
--
-- reverseWords :: String -> String
-- reverseWords = unwords . map reverse . words
