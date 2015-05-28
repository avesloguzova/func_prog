module Combinators
    ( module Parser
    , many, many1
    , symbol, anySymbol, string, oneOf
    , digit, natural, integer
    , spaces
    , try
    , endBy, endBy1
    , sepBy, sepBy1
    , foldr1P, foldl1P
    , between, brackets, parens, braces, angles
    ) where

import Parser
import Data.Char

-- Поведение всех комбинаторов описано в тестах в Main.hs.

-- (0.5 балла)
--satisfy :: (lex -> Bool) -> Parser lex lex
--
symbol :: Eq lex => lex -> Parser lex ()
symbol l =  pure (\_ -> ()) <*> satisfy (== l) 

-- (0.5 балла)
anySymbol :: Parser lex lex
anySymbol = satisfy (const True) 

-- (0.5 балла)
digit :: Parser Char Int
digit = pure (\x -> read [x]) <*> satisfy isDigit

-- (0.5 балла)
string :: Eq lex => [lex] -> Parser lex ()
string [] = pure ()
--satisfy(==x) :: Parser lex lex
--string xs :: Parser lex ()
-- <*> :: Parser lex (a -> b) -> Parser lex a -> Parser lex b
string  (x:xs) = pure (\_ _ -> ()) <*> satisfy (==x) <*> string xs


-- (0.5 балла)
oneOf :: Eq lex => [lex] -> Parser lex lex
oneOf [] = empty
oneOf (x:xs) = satisfy (==x) <|> oneOf xs

-- (0.5 балла)
many :: Parser lex a -> Parser lex [a]
many p = many1 p <|> pure []

-- (0.5 балла)
many1 :: Parser lex a -> Parser lex [a]
many1 p = pure (\h t -> h : t) <*> p <*> many p

-- (0.5 балла)
natural :: Parser Char Integer
natural = pure (\ns -> read ns) <*> many1 (satisfy isDigit)

-- (0.5 балла)
integer :: Parser Char Integer
integer = pure(\_ n -> (-n) ) <*> satisfy (=='-') <*> natural <|> natural

-- (0.5 балла)
spaces :: Parser Char ()
spaces = pure (const ()) <*> many (satisfy isSpace)

-- (0.5 балла)
try :: Parser lex a -> Parser lex (Maybe a)
try p = pure (\a -> Just a) <*> p <|> pure Nothing

-- (0.5 балла)
endBy :: Parser lex a -> Parser lex b -> Parser lex [a]
endBy lexeme sep = endBy1 lexeme sep <|> pure []

-- (0.5 балла)
endBy1 :: Parser lex a -> Parser lex b -> Parser lex [a]
endBy1 lexeme sep = many1 (pure (\a  _ -> a) <*> lexeme <*> sep)

-- (0.5 балла)
sepBy :: Parser lex a -> Parser lex b -> Parser lex [a]
sepBy lexeme sep = sepBy1 lexeme sep <|> pure [] 

-- (0.5 балла)
sepBy1 :: Parser lex a -> Parser lex b -> Parser lex [a]
sepBy1 lexeme sep = pure (\a  _  a' as -> a:a':as) <*> lexeme <*> sep <*> lexeme <*> many (pure (\_  a -> a) <*> sep <*> lexeme)

-- (0.1 балла)
between :: Parser lex a -> Parser lex b -> Parser lex c -> Parser lex c
between a b c = pure (\_ c _ -> c) <*> a <*> c <*> b

-- (0.1 балла)
brackets :: Parser Char a -> Parser Char a
brackets = between (symbol '[') (symbol ']') 

-- (0.1 балла)
parens :: Parser Char a -> Parser Char a
parens = between (symbol '(') (symbol ')') 

-- (0.1 балла)
braces :: Parser Char a -> Parser Char a
braces = between (symbol '{') (symbol '}') 

-- (0.1 балла)
angles :: Parser Char a -> Parser Char a
angles = between (symbol '<') (symbol '>') 

-- (1 балл)
foldr1P :: (a -> b -> a -> a) -> Parser lex a -> Parser lex b -> Parser lex a
foldr1P f p1 p2 = pure f' <*> p1 <*> (many (pure (\a b -> (a,b)) <*> p2 <*> p1)) where
    f' acc [] = acc
    f' acc ((b,a):c) = f acc b (f' a c)

-- (1 балл)
foldl1P :: (a -> b -> a -> a) -> Parser lex a -> Parser lex b -> Parser lex a
foldl1P f p1 p2 = pure f' <*> p1 <*> (many (pure (\a b -> (a,b)) <*> p2 <*> p1)) where
    f' acc [] = acc
    f' acc ((b,a):c) = f' (f acc b a) c
