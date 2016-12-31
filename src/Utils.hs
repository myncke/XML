module Utils where

println :: String -> String
println "" = ""
println (x:xs)
  | x == 'p' = xs
  | otherwise = ""
