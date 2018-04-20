module Main where

import System.Environment
import System.IO

import Parser
import Processor

main = do
  args <- getArgs
  case args of
    [file] -> parseFile file
    _      -> putStrLn "Wrong number of arguments"

parseFile :: String -> IO()
parseFile path = do
  file <- readFile path;
  statements <- parse $ file
  case statements of
    Nothing -> putStrLn "Error while parsing!"
    Just s -> do
      print statements
      process s
