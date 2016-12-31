{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import System.Environment
import Data.Char
import System.IO

-- test dingenn
import Control.Monad.State
import Control.Applicative

-- import Parser
import ParseData
import Processor



main = do
  args <- getArgs
  case args of
    [file] -> parseFile file
    _      -> putStrLn "Wrong number of arguments"

parseFile :: String -> IO()
parseFile path = do
  file <- readFile path;
  results <- process (lines file)
  return results


parseFile' :: String -> IO ()
parseFile' path = do
  withFile path ReadMode (\handle -> do
  contents <- hGetContents handle
  parse' contents)


parse' :: String -> IO ()
parse' fileContents = do
    let parsedLines = lines fileContents
        nrLines = zipWith (\n line -> show n ++ ".  " ++ line) [1..] parsedLines
    putStr $ unlines nrLines


newtype Parser a = Parser { runParser :: State String a }
    -- deriving ( Functor, Monad, Applicative, Alternative )
