module Main where

import System.Environment
import Parser
import ParseData
import Data.Char
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
