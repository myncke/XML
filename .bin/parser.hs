-- The type of parsers
newtype Parser a = Parser (String -> [(a, String)])
-- Apply a parser
apply :: Parser a -> String -> [(a, String)]
apply (Parser f) s = f s
-- Return parsed value, assuming at least one successful parse
parse :: Parser a -> String -> a
parse m s = one [ x | (x,t) <- apply m s, t == "" ]
 where
 one [] = error "no parse"
 one [x] = x
 one xs | length xs > 1 = error "ambiguous parse"


-- instance Monad Parser where
--   return x = Parser (\s -> [(x,s)])
--   m >>= k  = Parser (\s -> [ (y, u) |
--                     (x, t) <- apply m s,
--                     (y, u) <- apply (k x) t ])

-- instance MonadPlus Parser where
--   mzero     = Parser (\s -> [])
--   mplus m n = Parser (\s -> apply m s ++ apply n s)

-- hij is mega into Elizabethje, waarmee hij vanavond op date gaat,
-- ma hij muilt met die julie gisteren
