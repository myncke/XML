
-- XML: Uitbreidbare Mats taaL

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
  -- print  file
  statements <- parse $ file
  case statements of
    Nothing -> putStrLn "Error while parsing!"
    Just s -> do
      print statements
      process s





-- parseFile' :: String -> IO ()
-- parseFile' path = do
--   withFile path ReadMode (\handle -> do
--   contents <- hGetContents handle
--   parse' contents)
--
--
-- parse' :: String -> IO ()
-- parse' fileContents = do
--     let parsedLines = lines fileContents
--         nrLines = zipWith (\n line -> show n ++ ".  " ++ line) [1..] parsedLines
--     putStr $ unlines nrLines
